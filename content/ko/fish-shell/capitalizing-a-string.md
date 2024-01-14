---
title:    "Fish Shell: 문자열 대문자 변환하기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# 왜 capitalizing a string을 할까요?

문자열을 대문자로 변환하는 것은 언어나 프로그래밍에서 매우 일반적인 작업입니다. 이 작업을 수행하는 이유는 주로 사용자가 쉽게 읽을 수 있도록 문자열을 정리하고 일관된 형식으로 표시하기 위해서입니다. 또는 문자열을 다른 프로그램이나 함수로 전달할 때 이를 인식하기 쉽게 하기 위해서입니다.

# Capitalizing a string 하는 방법

만약 Fish Shell을 사용한다면, 문자열을 대문자로 변환하기 위해 `string capitalize` 함수를 사용할 수 있습니다.

```
Fish Shell > string capitalize "hello world"
Hello world
```

또는 문자열 변수를 먼저 만들어서 `string capitalize` 함수를 이용해 대문자로 변환할 수도 있습니다.

```
Fish Shell > set greeting "hello world"
Fish Shell > string capitalize $greeting
Hello world
```

# Deep Dive

Fish Shell의 `string capitalize` 함수는 문자열의 첫 글자를 대문자로 만들고, 나머지 문자열은 소문자로 만듭니다. 만약 첫 글자가 정의되지 않은 문자(Rune)라면, 메시지 "Invalid character in string"을 출력합니다.

이를 발생하지 않도록 하기 위해서는 먼저 문자열에 대해 검증 작업을 수행하고, 첫 글자가 무엇인지에 따라 대소문자를 구분해야 합니다.

# See Also

- [Fish Shell 문자열 관련 함수](https://fishshell.com/docs/current/commands.html#string)
- [Fish Shell 문자열 읽기](https://fishshell.com/docs/current/tutorial.html#reading-a-string)
- [Fish Shell 문자열 다루기](https://fishshell.com/docs/current/tutorial.html#string-operations)