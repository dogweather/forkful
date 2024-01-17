---
title:                "문자열의 길이 찾기"
html_title:           "Fish Shell: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 뭐고 왜?: 
문자열 길이를 찾는 것은 문자열에 포함된 문자의 개수를 파악하는 것입니다. 프로그래머들은 이 작업을 일반적으로 문자열을 다루는 프로그래밍 작업에서 사용하기 때문에 수행합니다.

## 방법: 
Fish Shell에서 문자열 길이를 찾는 방법은 간단합니다. 다음은 여러분이 따라할 수 있는 예제와 출력입니다.
```Fish Shell
# 변수에 문자열 값 저장
set my_string "Hello World"
# "count" 함수를 사용하여 문자열 길이 찾기
echo (count $my_string)
```
출력:
```
11
```

## 자세히 살펴보기: 
(1) 문자열 길이를 찾는 작업은 프로그래밍의 일부로서 오랜 역사를 가지고 있습니다. 초기에는 C 언어에서 사용되던 "strlen" 함수로 이 작업을 수행할 수 있었고, 현재 다양한 프로그래밍 언어에서도 비슷한 함수를 제공합니다. (2) 다른 언어들에서는 "length" 또는 "size" 함수를 사용하여 문자열 길이를 찾는 경우도 많습니다. (3) Fish Shell의 "count" 함수는 문자열을 문자 단위로 쪼개서 그 개수를 세는 방식으로 문자열 길이를 찾습니다.

## 관련 링크: 
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/cmds/count.html)
- [C 언어에서 문자열 길이 찾기](https://www.programiz.com/c-programming/library-function/string.h/strlen)
- [다른 언어에서 문자열 길이 찾기의 다양한 방법](https://www.techonthenet.com/sql_server/functions/len.php)