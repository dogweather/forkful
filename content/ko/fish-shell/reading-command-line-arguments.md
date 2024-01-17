---
title:                "명령 줄 인자 읽기"
html_title:           "Fish Shell: 명령 줄 인자 읽기"
simple_title:         "명령 줄 인자 읽기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
커맨드 라인 인수를 읽는 것은 프로그래머가 커맨드 라인에서 사용자로부터 입력 값을 읽어오는 것입니다. 이것은 프로그램을 더 유연하고 유용하게 만들어주며, 사용자와 상호작용하는 데 도움이 됩니다.

## 하는 방법:
```Fish Shell``` 코드 블록 내에서 코드 예제와 샘플 출력을 확인할 수 있습니다. 
우선, ```status``` 명령어를 사용해 현재 디렉토리와 파일을 확인합니다. 그 후, ```grep```을 사용하여 원하는 파일을 찾아내고, ```wc -l```를 사용하여 해당 파일의 라인 수를 세어봅니다.

## 더 자세히:
커맨드 라인 인수를 읽는 것은 프로그래밍 언어마다 동작 방식이 다를 수 있습니다. 가장 일반적인 방법은 ```argc```와 ```argv``` 변수를 사용하는 것입니다. 또한, ```getopt```와 같은 라이브러리를 사용하여 더욱 복잡한 옵션을 처리할 수도 있습니다.

## 더 알아보기:
[Fish Shell의 공식 문서](https://fishshell.com/docs/current/cmds/read.html)를 참조하시면 더 많은 정보를 얻을 수 있습니다. 또한, 다른 쉘에서도 커맨드 라인 인수를 다루는 방법을 비교해 볼 수 있습니다.