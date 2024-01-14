---
title:                "Fish Shell: 문자열 추출하기"
simple_title:         "문자열 추출하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?

문자열에서 일부 문자열을 추출하는 과정은 프로그래밍에서 자주 필요한 작업입니다. 예를 들어, 특정 패턴을 따르는 파일 이름을 필터링하는 경우, 일부 문자열을 추출하고 그것을 비교하는 것이 효과적일 수 있습니다. 또한, 추출한 문자열을 변수로 사용하여 자동화된 작업을 수행할 수도 있습니다.

## 어떻게?

Fish Shell은 문자열 추출을 간단하고 쉽게 처리하는 기능을 제공합니다. 다음은 간단한 예제 코드와 함께 추출 방법을 보여줍니다.

```Fish Shell
# First, define a string variable
set my_string "Hello World"

# Use the `string sub` command to extract a substring
string sub -s 6 $my_string    # This will output "World"
```

파일 이름을 필터링하는 예제에서, `string sub` 명령어를 사용하여 파일의 확장자를 추출하고 그것을 비교할 수 있습니다.

```Fish Shell
# Define a variable for the file name
set file_name "my_document.txt"

# Extract the file extension using `string sub`
string sub -bse 5 $file_name   # This will output "txt"
```

## 딥 다이브

추출한 문자열을 사용할 때 유용한 옵션과 플래그가 있습니다. 다음은 몇 가지 예시입니다.

- `-s` 옵션: 추출할 부분의 시작 위치를 지정합니다.
- `-e` 옵션: 추출할 부분의 끝 위치를 지정합니다.
- `-b` 옵션: 추출할 부분의 시작 위치를 역순으로 지정합니다.
- `-w` 옵션: 추출할 부분의 문자 수를 지정합니다.

또한, 상황에 따라 정규식을 사용하여 문자열을 추출할 수도 있습니다. 이 기능을 사용하면 더 많은 유연성을 가지고 추출 작업을 수행할 수 있습니다.

## 더 알아보기

더 자세한 내용을 알고 싶다면 공식 Fish Shell 문서를 참조하세요. https://fishshell.com/docs/current/cmds/string.html

## 관련 링크

- Fish Shell 사용 방법: https://medium.com/home-decor-hacks/the-power-of-fish-shell-e942a5fac4b7
- 문자열 처리를 위한 정규식 튜토리얼: https://regexone.com/