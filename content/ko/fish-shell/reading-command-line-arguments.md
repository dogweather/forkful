---
title:                "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
html_title:           "Fish Shell: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜
Fish 셸에서 명령 줄 인수를 읽는 방법을 알려주는 이 기사를 읽는 이유는 무엇일까요? 일단 간단하게 말씀드리자면, 명령 줄 인수는 컴퓨터와의 대화를 할 때 매우 유용한 도구입니다. 커맨드 라인에서 작업을 할 때 매번 우리가 원하는 결과를 얻기 위해 프로그램을 다시 실행하는 것은 매우 번거롭기 때문에, 명령 줄 인수를 잘 이해하고 사용하는 것은 중요합니다.

## 방법
우리가 Fish 셸에서 명령 줄 인수를 읽을 때 어떤 방법을 사용해야 할까요? 답은 간단합니다! `argv` 변수를 사용하는 것이죠. `argv` 변수는 명령 줄에서 실행된 프로그램의 이름과 추가 인수들을 포함한 배열(array)입니다. 예를 들어, `ls -l`이라는 명령을 실행하면 `argv` 배열에는 `["ls", "-l"]` 이라는 값이 저장됩니다.

```Fish Shell
set argv "ls -l"
for arg in $argv
    echo $arg
end
```

위의 예제에서는 `$argv` 변수를 `for` 루프를 사용하여 출력하는 방법을 보여줍니다.

```Fish Shell
ls
-l
```

위와 같이 출력되는 것을 볼 수 있습니다. 이제 당신은 명령 줄 인수를 다루는 간단한 예제를 따라 해 보면서 더 자세히 살펴보겠습니다.

## 깊게 들어가기
`argv` 배열에는 프로그램의 이름과 추가 인수들이 들어가는 것 외에도, Fish 셸에서는 명령 줄에서 옵션을 효율적으로 처리하기 위해 여러 가지 방법을 제공합니다. 예를 들어, `-f`라는 옵션을 추가로 받고 싶다면 `has-option` 함수를 사용하여 `$argv` 배열에서 해당 옵션이 있는지 확인할 수 있습니다.

```Fish Shell
set argv "ls -l -f index.html"
if has-option -- "f" $argv
    echo "Index file specified!"
else
    echo "No index file specified"
end
```

위 예제에서는 `-f` 옵션을 사용해서 `index.html` 파일을 명령 줄에서 따로 지정하는 것을 확인합니다. 그렇지 않을 경우, `No index file specified`라는 메시지가 출력됩니다.

## 참고자료
- [Fish Shell 공식 사이트] (https://fishshell.com/)
- [Fish Shell Github] (https://github.com/fish-shell/fish-shell)
- [Fish 셸 스크립트 예제] (https://fishshell.com/docs/current/tutorial.html)