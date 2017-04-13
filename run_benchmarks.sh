for i in {16..16}
do
    # echo "---"
    # echo "> fib, $i threads"
    # erl +S $i -noshell -s benchmark test_fib -s init stop > output-fib-$i.txt
    # echo "---"
    # echo "> get_channel_history, $i threads"
    # erl +S $i -noshell -s benchmark test_get_channel_history -s init stop > output-get_channel_history-$i.txt
    echo "> send_message, $i threads"
    erl +S $i -noshell -s benchmark test_send_message -s init stop > output-send_message-$i.txt

    echo "---"

    echo "final benchmarks $i threads"
    erl +S $i -noshell -s final_benchmarks main -s init stop > benchmarks-$i.txt
    
    echo "---"
done
