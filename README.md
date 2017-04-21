# 얼라희  
얼라희는 아희의 얼랭(erlang) 구현체입니다.  

## 실행  

    $ erlc erlaheui.erl
    $ erl -noshell -eval "erlaheui:c([아희파일]), init:stop()"

    경로를 \\"로 감싸는 걸 잊으면 안됩니다.

## [snippets](https://github.com/aheui/snippets) 테스트  
    $ ./test.sh  
    $ ...
    $ test status: 20 / 33  

## 개발환경  

얼라희는 다음 환경에서 개발하였습니다.  

|Erlang emulator|Erlang OTP|
|---------------|----------|
|5.10.4         |R16B03-1  |  

