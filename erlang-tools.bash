export PATH=~/.erlang-tools:$PATH

source ~/.erlang-tools/kerl.bash

function erlang {
    source ~/erlang/otp/$1/activate
}

function erl {
    local node_name=$(basename $PWD)
    command erl -config ~/erl_hist.config -name $node_name@127.0.0.1 -pa $PWD/ebin -pa $PWD/apps/*/ebin -pa $PWD/deps/*/ebin $@
}
