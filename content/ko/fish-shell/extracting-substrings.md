---
title:                "부분 문자열 추출하기"
html_title:           "Fish Shell: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

깊은 곳까지 들어가 보자!
## 무엇 & 왜?
문자열 추출이란 무엇인가요? 프로그래머들이 왜 그것을 하는 걸까요?

문자열 추출은 주어진 문자열에서 특정 부분의 일부 혹은 전체를 추출하는 작업입니다. 프로그래머들이 이 작업을 하는 이유는 특정 정보를 추출하여 처리하거나, 입력된 데이터를 정확히 다루기 위해서 입니다.

## 방법:
```Fish Shell```의 코드 블록 안에 코딩 예제와 샘플 출력을 포함하였습니다.

**추출한 부분 문자열을 대문자로 변환하기:**
```
set str "hello world"
echo $str[2,5] | tr a-z A-Z
```
출력: ```ELLO```

## 깊게 들어가기:
문자열 추출에 대한 역사적인 배경, 대안들 및 구현 세부 사항과 같은 깊은 정보를 살펴보겠습니다.

우선, 기존에 ```grep```이나 ```sed```와 같은 다른 쉘 명령어를 사용하여 문자열 추출을 할 수도 있지만, 그것들은 조금 더 복잡한 구문을 필요로 합니다. 그에 비해 ```Fish Shell```은 간단한 구문을 통해 쉽게 문자열 추출을 할 수 있기 때문에 더 편리합니다. 또한, ```Fish Shell```은 특수 문자를 이용한 문자열 추출도 가능합니다.

## 관련 자료:
문자열 추출에 대한 관련 자료들입니다.

- [Official Fish Shell Documentation](https://fishshell.com/docs/current/cmds/set.html)
- [Fish Shell Tutorial](https://devmanual.gentoo.org/tasks-reference/shell-fish/index.html)