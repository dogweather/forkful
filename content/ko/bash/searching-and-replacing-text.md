---
title:                "텍스트 검색 및 대체"
html_title:           "Bash: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜
많은 사람들이 텍스트를 검색하고 바꾸는 일에 참여하는 이유는 간단합니다. 우리는 모두 작업 또는 프로젝트에서 일부 텍스트를 반복하고 수정해야 할 때가 있습니다. 이러한 작업은 매우 번거롭고 시간이 오래 걸리지만 베이시스크립트를 사용하면 텍스트 검색 및 교체 프로세스를 자동화하여 더 효율적으로 수행 할 수 있습니다.

## 방법
우리는 검색 및 교체를 할 수 있는 많은 명령어와 옵션을 가지고 있지만 그 중 몇 가지만 살펴보겠습니다. 먼저 텍스트 검색에 자주 사용되는 `grep` 명령어를 살펴봅시다. 예를 들어, 현재 디렉토리에서 "hello"라는 단어를 포함하는 모든 파일을 찾으려면 다음과 같이 입력합니다:

```Bash
grep "hello" *
```

또는 "hello"라는 단어를 가진 모든 파일의 라인 수를 세려면 다음과 같이 입력합니다:

```Bash
grep -c "hello" *
```

텍스트를 검색 한 후에는 이를 교체하는 것도 매우 쉽습니다. `sed` 명령어를 사용하면 텍스트를 교체할 수 있습니다. 예를 들어, "hello"를 "hi"로 대체하려면 다음과 같이 입력합니다:

```Bash
sed -i 's/hello/hi/g' file.txt
```

마지막으로, `awk` 명령어를 사용하면 텍스트를 다른 형식으로 변경하거나 특정 조건에 따라 텍스트를 선택할 수도 있습니다. 예를 들어, "hello"를 포함하는 파일의 첫 번째 단어를 선택하려면 다음과 같이 입력합니다:

```Bash
awk '/hello/ {print $1}' file.txt
```

## 딥 다이브
우리는 검색 및 교체를 위해 더 많은 명령어와 옵션을 다룰 수 있지만 여기서는 간단한 예제만 살펴보았습니다. 그러나 더 깊이 들어가서 배울 수 있는 많은 기능이 있습니다. 예를 들어, 정규식을 사용하면 더 복잡한 검색 및 대체 작업을 할 수 있습니다. 또한 `sed`와 `awk`를 함께 사용하면 텍스트를 원하는 대로 다양한 방식으로 조작할 수 있습니다.

## 참고
* [Bash 검색 및 교체 가이드](https://www.gnu.org/software/sed/manual/html_node/)
* [시작하세요! Awk: Text 처리의 고급 프로그래밍 언어](https://www.gnu.org/software/gawk/manual/gawk.html)
* [정규식 테스트 및 연습](https://regex101.com/)