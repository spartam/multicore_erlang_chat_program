mkdir benchmarks

for i in {1..8}
do
    # echo "---"
    # echo "> fib, $i threads"
    # erl +S $i -noshell -s benchmark test_fib -s init stop > output-fib-$i.txt
    # echo "---"
    # echo "> get_channel_history, $i threads"
    # erl +S $i -noshell -s benchmark test_get_channel_history -s init stop > output-get_channel_history-$i.txt
    # echo "> send_message, $i threads"
    # erl +S $i -noshell -s benchmark test_send_message -s init stop > output-send_message-$i.txt

    # echo "---"

    echo "final benchmarks $i threads going to 8 threads"
    for j in {1..50}
    do
    	echo "run $j of 50 for statistical relevance"
    	erl +S $i -noshell -s final_benchmarks main -s init stop >> benchmarks/benchmarks-$i.txt
    done
    
    echo "---"
done
