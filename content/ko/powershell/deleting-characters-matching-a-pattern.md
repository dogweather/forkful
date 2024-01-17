---
title:                "패턴과 일치하는 문자 지우기"
html_title:           "PowerShell: 패턴과 일치하는 문자 지우기"
simple_title:         "패턴과 일치하는 문자 지우기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

 문자 패턴과 일치하는 문자를 삭제하는 것은 프로그래밍 과정에서 자주 사용되는 작업입니다. 이를 통해 원하지 않는 데이터를 제거하거나, 특정 패턴을 가진 데이터를 필터링하는 등의 용도로 사용됩니다. 따라서 프로그래머들은 이 작업을 자주 시행합니다.

## 방법:

```powershell
# 문자 패턴과 일치하는 문자를 삭제하는 예제
$text = "Hello World!"
$text -replace "[aeiou]",""
```

이 코드의 결과는 "Hll Wrld!"입니다. [aeiou]는 모음을 나타내며, 이를 빈 문자열로 대체함으로써 해당 모음을 삭제하였습니다.

## 깊은 곳

삭제 작업은 정규 표현식, 즉 패턴 매칭에 기반합니다. 이는 프로그래밍이 아닌 문자열 처리 작업에서도 많이 사용되는 개념입니다. 따라서 이 작업을 통해 문자열 데이터를 간편하게 조작할 수 있습니다.

대부분의 프로그래밍 언어에서는 정규 표현식을 지원하고 있으며, 삭제 작업에 대한 다양한 방법이 존재합니다. PowerShell에서는 -replace 명령어를 통해 간단하게 문자 삭제 작업을 수행할 수 있습니다.

## 참고 자료

- [정규 표현식 기초](https://www.begoode.com/coding/regex/)
- [PowerShell 공식 문서](https://docs.microsoft.com/en-us/powershell/scripting/learn/regular-expressions-in-powershell?view=powershell-7.1)