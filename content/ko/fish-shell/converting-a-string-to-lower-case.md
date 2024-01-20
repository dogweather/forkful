---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자열을 소문자로 변환하는 것은 그 대소문자 구분 없이 같은 단어로 인식하도록 하는 작업입니다. 대소문자를 무시하고 문자열을 비교하거나 정렬하려면 이런 변환이 필요합니다.

## 사용법: 

Fish Shell에서 문자열을 소문자로 변환하는 방법을 보여드릴게요.

```Fish Shell
> set string "Hello, World!"
> echo $string | tr '[:upper:]' '[:lower:]'
hello, world!
```
이런 식으로 `tr` 명령어를 사용합니다.

## 깊이 들어가보기: 

대소문자 변환 기능은 초기 컴퓨팅 시스템의 탄생부터 있었습니다. 그때부터 개발자들은 사용자의 입력이나 데이터를 표준화하기 위해 이를 많이 사용했습니다.

다른 대안으로 Bash shell에서는 `tr` 외에도 `awk`나 `sed` 같은 도구를 사용할 수 있습니다.

Fish Shell에서는 내부적으로 `tr`이 POSIX character classes를 사용하여 작동합니다. 이 클래스는 '[[:upper:]]'와 '[[:lower:]]'같은 형태로 표현되며, 알파벳 대문자와 소문자를 나타냅니다.

## 참고자료:

Fish Shell의 공식 문서를 확인해보세요. 여기서 많은 정보를 얻을 수 있습니다. 
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [POSIX character class](https://en.wikipedia.org/wiki/Regular_expression#POSIX_character_classes)