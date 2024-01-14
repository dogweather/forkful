---
title:                "Fish Shell: 컴퓨터 프로그래밍 기사 제목: 명령줄 인수 읽기"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

그래 프로그래밍을 할 때 `Fish Shell`을 사용하는 걸 추천한다. 이 명령 줄 인수에 대해 배울 수 있기 때문이다.

## 사용 방법

만약 당신이 `Fish Shell` 사용자라면, 당신은 이제 콘솔에서 명령을 입력할 때 사용할 수있는 강력한 도구를 가지게 될 것입니다. 그 중 하나가 명령 줄 인수를 읽는 것입니다. 이를 통해 당신은 쉽게 다양한 작업을 수행 할 수 있으며 코드를 더욱 효율적으로 작성할 수 있습니다.

### 파일 이름 읽기
```Fish Shell
cat $ARGV[1]
```
위의 코드는 입력으로 받은 파일의 이름을 가져와서 `cat` 명령어를 사용하여 내용을 출력합니다. 파일의 이름은 `$ARGV[1]`로 지정할 수 있습니다.
```
$ python myscript.py file.txt
Hello world!
```

### 인수를 사용한 조건문
```Fish Shell
if test $1 = "on"; echo "Turned on"; end
```
위의 코드는 첫번째 인수가 "on"인 경우, "Turned on"이라는 문자열을 출력하고, 그렇지 않은 경우 아무것도하지 않는 간단한 조건문 예제입니다.

## 깊게 파헤치기

명령 줄 인수에 대해 더 자세히 알고 싶다면, `Fish Shell Handbook`를 참조하십시오. 이 문서에는 `Fish Shell`의 모든 기능과 명령 줄 인수를 읽는 더 많은 예제가 포함되어 있습니다.

## 관련 문서

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell Handbook](https://fishshell.com/docs/current/index.html)
- [Using Command Line Arguments in Fish Shell](https://www.linuxjournal.com/content/using-command-line-arguments-fish-shell)
- [BASH command line arguments](https://wiki.bash-hackers.org/scripting/posparams)