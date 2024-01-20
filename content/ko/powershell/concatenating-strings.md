---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 연결이란 두 개 혹은 그 이상의 문자열을 하나로 합치는 것을 말합니다. 개발자들이 이를 활용하는 이유는 데이터를 보다 명확하고 읽기 쉽게 표현하기 위해서입니다.

## 사용 방법:

PowerShell에서 문자열을 연결하는 가장 간단한 방법은 두 문자열 사이에 '+' 연산자를 사용하는 것입니다.

```PowerShell
$string1 = "안녕"
$string2 = "하세요"
$string3 = $string1 + " " + $string2
echo $string3
```
출력 :

```
안녕 하세요
```

또 다른 방법으로는 `-f` 연산자와 함께 서식 문자열을 사용하는 방법이 있습니다:

```PowerShell
$arg1 = "안녕하세요,"
$arg2 = "PowerShell!"
$string = "{0} {1}" -f $arg1, $arg2
echo $string
```
출력 :

```
안녕하세요, PowerShell!
```

## Deep Dive:

문자열 연결은 컴퓨터 프로그래밍의 근간이며, 앞서 지적한 바와 같이 데이터를 보다 읽기 쉽게 표현하는데 사용됩니다. 하지만, PowerShell에서 다른 언어와 스타일대로 문자열을 연결하면 추가 메모리 할당과 GC (Garbage Collection) 로 인해 성능 하락을 가져올 수 있습니다.
더 나은 성능을 위해 `-f` 방법 또는 StringBuilder 클래스를 사용하는 것이 좋습니다.

## 참고 자료:

- StackOverflow : [문자열 연결 - PowerShell](https://stackoverflow.com/questions/8475232/how-to-concatenate-strings-and-variables-in-powershell)