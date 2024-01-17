---
title:                "문자열 삽입하기"
html_title:           "Fish Shell: 문자열 삽입하기"
simple_title:         "문자열 삽입하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 보간이란 무엇인가요? 프로그래머들이 왜 이를 사용할까요?

문자열 보간은 변수나 표현식을 문자열 안에 삽입하는 것을 말합니다. 프로그래머들은 이를 사용하여 코드를 더 간결하게 만들고, 동적인 문자열을 생성할 수 있습니다.

## 방법:
`Fish Shell`의 코드 블록인 ```...``` 안에 코딩 예제와 샘플 출력을 포함시켜 보겠습니다.

### 변수 보간
```fish
set name "Andy"
echo "Hello, $name! How are you?"
```
```
Hello, Andy! How are you?
```

### 수식 보간
```fish
set num1 2
set num2 3
echo "The sum of $num1 and $num2 is ($num1 + $num2)."
```
```
The sum of 2 and 3 is (2 + 3).
```

## 깊게 들어가보기:
1. 역사적 배경: 문자열 보간은 Perl 언어에서 비롯되었으며, 다른 프로그래밍 언어들에서도 널리 사용됩니다.
2. 대안: 문자열 보간 대신 `printf`를 사용할 수도 있지만, 코드를 더 복잡하게 만들 수 있습니다.
3. 구현 세부 정보: `Fish Shell`에서는 내장된 `string` 함수를 사용하여 문자열 보간을 합니다.

## 관련 자료:
- [Fish Shell 공식 웹사이트](https://fishshell.com/)
- [Perl 언어의 문자열 보간에 대한 역사](https://www.perl.com/pub/1998/08/string_internals.html/)