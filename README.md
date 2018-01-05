얼라희  
========
얼라희는 [아희](http://aheui.github.io/)의 얼랭(erlang) 구현체입니다.  

## 빌드

    $ rebar3 compile

## 실행

    $ rebar3 shell
    ===> Verifying dependencies...
    ===> Compiling erlaheui
    Eshell V8.3 (abort with ^G)
    1> erlaheui:c("../snippets/hello-world/hello-world.puzzlet.aheui").
    Hello, world!
    {ok, 0}

## 테스트
아희 표준 [snippets](https://github.com/aheui/snippets) 테스트  

    $ ./test.sh [테스트셋 = standard]
    test bieup-char...success!
    test bieup-sign...success!
    test bieup...success!
    ...
    test status: 31/33
    $ 

|# |테스트코드          |통과              |
|--|--------------------|------------------|
|1 |bieup-char          |:heavy_check_mark:|
|2 |bieup-sign          |:heavy_check_mark:|
|3 |bieup               |:heavy_check_mark:|
|4 |border              |:heavy_check_mark:|
|5 |chieut              |:heavy_check_mark:|
|6 |default-direction   |:heavy_check_mark:|
|7 |default-storage     |:heavy_check_mark:|
|8 |digeut              |:heavy_check_mark:|
|9 |emptyswap           |N/A|
|10|exhausted-storage   |:heavy_check_mark:|
|11|exitcode            |N/A|
|12|hieut-pop           |:heavy_check_mark:|
|13|ieunghieut          |:heavy_check_mark:|
|14|jieut               |:heavy_check_mark:|
|15|loop                |:heavy_check_mark:|
|16|mieum               |:heavy_check_mark:|
|17|nieun               |:heavy_check_mark:|
|18|pieup               |:heavy_check_mark:|
|19|print               |:heavy_check_mark:|
|20|queue               |:heavy_check_mark:|
|21|rieul               |:heavy_check_mark:|
|22|shebang             |:heavy_check_mark:|
|23|ssangbieup          |:heavy_check_mark:|
|24|ssangdigeut         |:heavy_check_mark:|
|25|ssangsiot           |:heavy_check_mark:|
|26|storage             |:heavy_check_mark:|
|27|syllable            |:heavy_check_mark:|
|28|tieut               |:heavy_check_mark:|
|29|vowel-2step         |:heavy_check_mark:|
|30|vowel-advanced      |:heavy_check_mark:|
|31|vowel-basic         |:heavy_check_mark:|
|32|vowel-useless       |:heavy_check_mark:|
|33|vowel-useless2      |:heavy_check_mark:|
|34|quine.puzzlet.40col |:heavy_check_mark:|
|35|quine.puzzlet       |:heavy_check_mark:|
|36|pi.puzzlet          |:heavy_check_mark:|

## 개발환경  

얼라희는 다음 환경에서 개발하였습니다.  

|Erlang emulator|Erlang OTP|
|---------------|----------|
|5.10.4         |R16B03-1  |  
