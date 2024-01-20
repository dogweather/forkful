---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 검색 및 교체는 특정 문자열을 찾고 다른 것으로 대체하는 프로그래밍 기술입니다. 프로그래머는 버그 수정, 코드 수정, 데이터 변환 등을 위해 이를 수행합니다.

## 어떻게하는 방법:

Fish shell에서 텍스트를 검색하고 바꾸려면 `string replace` 명령을 사용합니다. 다음은 간단한 예시입니다:

```Fish Shell
> set sentence "Hello, world!"
> string replace "world" "Fish user" -- $sentence
Hello, Fish user!
```

위의 코드는 "world"을 "Fish user"로 대체합니다.

## 깊게 탐구:

Fish Shell의 `string replace`는 UNIX의 전통적인 `sed`과 `awk` 도구에 영감을 받아 개발되었습니다. 하지만 Fish는 더 명확하고 직관적인 문법을 목표로 합니다.

대안으로는 `sed` 또는 `awk`를 사용할 수도 있지만, 이들은 사용법이 복잡하고 초기 학습 곡선이 높을 수 있습니다. `string replace`는 더 간단하고 직관적인 방식으로 검색 및 교체 기능을 제공합니다.

Fish shell에서 `string replace`는 매우 효율적으로 구현되었습니다. 그것은 빠른 문자열 대체를 위해 Boyer-Moore 알고리즘을 사용합니다.

## 관련 내용 보기:

1. Fish Shell 공식 문서: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
2. 텍스트 검색 및 변환에 대한 자세한 설명: [https://en.wikipedia.org/wiki/String_searching_algorithm](https://en.wikipedia.org/wiki/String_searching_algorithm)
3. 더 깊게 배우기 위한 `sed`와 `awk`: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html), [https://www.gnu.org/software/gawk/manual/gawk.html](https://www.gnu.org/software/gawk/manual/gawk.html)