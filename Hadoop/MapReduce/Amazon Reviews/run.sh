hadoop fs -rm -r hdfs:///{ path }
hadoop jar hadoop-streaming-2.7.2.jar -D mapreduce.job.maps=20  -D mapreduce.job.reduce=2  -file mapper.py    -mapper mapper.py -file reducer.py   
-reducer reducer.py -input reviews.json -output debug
hadoop fs -getmerge { path } output
