---
title:                "문자열 대문자 변환"
html_title:           "PowerShell: 문자열 대문자 변환"
simple_title:         "문자열 대문자 변환"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열을 대문자로 바꾸는 것은 글자들을 모두 대문자로 변환하는 것을 말합니다. 프로그래머들은 일반적으로 문자열을 대문자로 변환하기 위해 사용합니다.

## 어떻게:

다음은 PowerShell을 사용하여 문자열을 대문자로 변환하는 예시입니다.

```
PowerShell "Hello, World!".ToUpper()
```

결과:

```
HELLO, WORLD!
```

문자열의 일부분만 대문자로 변환하기 위해서는 다음과 같이 하면 됩니다.

```
PowerShell "Hello, World!".Substring(0,5).ToUpper() + " World!"
```

결과:

```
HELLO World!
```

## 깊이 파고들기:

문자열을 대문자로 변환하는 것은 이전부터 프로그래밍에서 사용해온 기술입니다. 다른 대안으로 모든 문자를 소문자로 변환하는 방법도 있지만, 대문자로 변환하는 것은 간단하고 빠르게 사용할 수 있는 방법입니다. 이 문자열 변환 기술은 웹 개발에서 자주 사용되며, 다양한 언어에서 지원됩니다.

## 또한 참고:

- [PowerShell 문자열 작업 문서](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7)
- [PowerShell 문서 공식 홈페이지](https://docs.microsoft.com/en-us/powershell/?view=powershell-7)
- [PowerShell 코드 예제들](https://docs.microsoft.com/en-us/powershell/scripting/code-style/overview?view=powershell-7)