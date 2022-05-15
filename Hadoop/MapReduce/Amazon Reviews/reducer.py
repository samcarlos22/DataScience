#!/usr/bin/env python3
# -*- coding=utf-8 -*-
import sys
import re
from operator import itemgetter
from collections import Counter


def reducer():
    # Initialize the maps to store the necessary counters
    word_map = {}
    cat_map = []
    cat_word_map = []
    top_words = []
    prev_word = ''
    prev_cat = ''
    w_index = 0
    c_index = 0
    w_count = 0
    c_count = 0
    total = 0
    start = True

    # Read output from mapper (Word, Category, Counter)
    for line in sys.stdin:
        cat, word = line.strip('\n').split("$")

        try:
            # Initialize the variables for first run
            if start:
                prev_cat = cat
                prev_word = word
                start = False
            
            
            # Count the total occurence of each word
            if sys.intern(prev_word) is not sys.intern(word):
                if not word_map.get(prev_word):
                    word_map[prev_word] = (w_index, w_count)
                    cat_word_map.append((c_index, w_index, w_count, 0))
                    w_index += 1
                else:
                    w_tuple = word_map.get(prev_word)
                    word_map[prev_word] = (w_tuple[0], w_tuple[1] + w_count)
                    cat_word_map.append((c_index, w_tuple[0], w_count, 0))
                w_count = 0

            # Add the total occurence of each word in each category
            if sys.intern(prev_cat) is not sys.intern(cat):
                cat_map.append((prev_cat, c_count))
                c_index += 1
                c_count = 0

            prev_cat = cat
            prev_word = word
            c_count += 1
            w_count += 1
            total += 1
        except ValueError:
            pass
    
    # Add the words and category from the last counters
    if not word_map.get(word):
        word_map[word] = (w_index, w_count)
        cat_word_map.append((c_index, w_index, w_count, 0))
    else:
        w_tuple = word_map.get(word)
        word_map[word] = (w_tuple[0], w_tuple[1] + w_count)
        cat_word_map.append((c_index, w_tuple[0], w_count, 0))

    cat_map.append((cat, c_count))

    # Separate keys and values from word_map dictionary 
    word_map, word_list = zip(*word_map.items())
    
    
    index = 0

    # Calculate chi for all words in every category
    for c_index, w_index, w_count, chi in cat_word_map:
        
        (_, cv) = cat_map[c_index]
        (_, wv) = word_list[w_index]

        # Calculate the word frequency estimation
        # (Total Words in Category * Total Ocurrence of Current Word) / Total of all Words 
        est = (cv * wv) / total

        # Calculate chi
        chi = (w_count - est) ** 2 / est

        # Convert chi to int for memory efficiency
        cat_word_map[index] = (c_index, w_index, w_count, int(chi))
        index += 1

    # Get the count of each category inside cat_word_map list 
    # These values are used to crop the values based on category index
    # i.e cat_word_map[0:counter[0]] contains all values for category 0
    counter = Counter(elem[0] for elem in cat_word_map)
    counter = sorted(counter.most_common(), key=itemgetter(0))
    counter = [value for _, value in counter]
    
    # Counters for croping the cat_word_map list 
    i_counter = 0
    index_sum = 0

    for cat, _ in cat_map:
        if i_counter > len(counter):
            i_counter = 0
            index_sum = 0

        # Select all words belonging to the current category in the iteration, 
        # sort by chi and crop the 150 best values for each
        cat_top = sorted(cat_word_map[index_sum: index_sum + counter[i_counter]], key=itemgetter(3), reverse=True)[:150]
        cat_top = [(word_map[w_index], chi) for c_index, w_index, w_count, chi in cat_top ]
                    
        # Create a union of all the selected words from each category
        top_words = set.union(set(top_words), set(map(itemgetter(0), cat_top)))

        # Generate the output string in the format "word[0]:chi ... word[150]:chi"
        cat_top = ' '.join([str(k) + ":" + str(v) for k,v in cat_top])
                    
        # Send output to stdout in the format "Category  word[0]:chi ... word[150]:chi"
        print('%s\t%s' % (cat, cat_top))
        
        index_sum += counter[i_counter]
        i_counter += 1
        
    
    # Send the set of most representative words to stdout
    print(' '.join(i for i in sorted(top_words)))
    

reducer()
