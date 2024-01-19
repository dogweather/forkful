---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 이게 무엇이고 왜 사용하는가?

패턴에 일치하는 문자를 삭제하는 것은 텍스트 문자열에서 특정 패턴에 일치하는 모든 문자를 제거하는 과정입니다.이를 통해 불필요한 문자를 제거하고 데이터를 정리하거나, 분석 가능한 형식으로 변환하는데 사용됩니다.

## 어떻게 사용하나?

Bash에서는 tr 명령어와 함께 -d 옵션을 사용하여 문자를 삭제할 수 있습니다.

```Bash
echo "Hello World!" | tr -d '!'
```
이 코드를 실행하면 아래 출력이 나옵니다:

```Bash
Hello World
```

위 코드에서 '!' 문자가 "Hello World!" 문자열에서 제거된 것을 볼 수 있습니다.

## 깊이 들여다보기

이것의 역사는 UNIX 시스템의 초창기로 거슬러 올라갑니다. tr 명령어는 UNIX의 초기 버전부터 있었으며, 문자 변환 및 삭제에 사용되었습니다. 

전문가는 대체할 수 있는 방법을 사용할 것을 권장하며, sed, awk 또는 Perl을 사용해도 동일한 결과를 도출할 수 있습니다. 

tr -d 명령어의 내부 로직은 일련의 문자를 읽고, 제거를 위한 문자에 해당하는 문자열을 찾습니다. 일치하는 경우 이 문자는 출력에서 제외됩니다.

## 참고 자료

1. [Shell Scripting 튜토리얼](https://www.shellscript.sh/tutorials/)
2. [대체할 수 있는 방법: Sed](https://www.grymoire.com/Unix/Sed.html)
3. [Bash에서의 문자열 조작](https://tldp.org/LDP/abs/html/string-manipulation.html)