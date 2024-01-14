---
title:    "Bash: 문자열 연결"
keywords: ["Bash"]
---

{{< edit_this_page >}}

자바 프로그래밍의 기본 기술 중 하나는 문자열을 결합하는 것입니다.

## 왜

문자열을 결합하는 것은 프로그래밍에서 빠질 수 없는 중요한 요소입니다. 이 기술을 사용하면 여러 개의 문자열을 합쳐 하나의 문자열로 만들 수 있습니다. 예를 들어, 사용자의 이름과 인사말을 결합하여 "안녕하세요, 홍길동님"과 같은 문자열을 만들 수 있습니다. 이것은 사용자에게 더 친근한 인사를 전할 수 있게 해줍니다.

## 사용 방법

Bash에서 문자열을 결합하는 방법은 간단합니다. 먼저, 문자열을 변수에 저장해야 합니다. 그리고 다른 변수나 문자열과 결합하기 위해 `+` 기호를 사용합니다. 아래는 예시 코드와 결과입니다.

```Bash
name="홍길동"
greeting="안녕하세요, "
echo $greeting$name
```

출력 결과:

```
안녕하세요, 홍길동
```

위 예시에서는 두 개의 변수 `name`과 `greeting`을 결합하여 `안녕하세요, 홍길동`이라는 결과를 얻었습니다. 이와 같이 여러 개의 문자열을 결합할 수 있으며, 변수만 있을 뿐만 아니라 직접 문자열을 사용할 수도 있습니다.

## 깊이 들어가기

문자열을 결합하는 기술은 프로그래밍에서 매우 중요한 역할을 합니다. 그럼에도 불구하고 많은 초보 프로그래머들이 이 기술을 제대로 활용하지 못하는 경우가 있습니다. 이를 피하기 위해 결합할 변수나 문자열의 위치를 정확히 지정하는 것이 중요합니다. 예를 들어, 다음과 같이 코드를 작성하면 예상치 못한 결과가 나올 수 있습니다.

```Bash
name="홍길동"
echo 안녕하세요, $name
```

출력 결과:

```
꽝! 안녕하세요, 홍길동
```

변수를 따옴표로 묶지 않아서 생긴 문제입니다. 따옴표가 없는 경우에는 Bash가 스페이스나 특수 문자를 기준으로 변수를 구분하게 되므로, `안녕하세요,`는 하나의 매개변수로 인식되어 출력되고, `$name`은 하나의 문자열로 인식되어 따옴표가 없는 경우와 동일한 형태가 됩니다. 따라서 변수를 사용할 때는 따옴표로 묶는 것이 안정적인 방법입니다.

## 관련 페이지

* [Bash variables](https://www.tutorialspoint.com/unix/unix-using-variables.htm)
* [Concatenating strings in Bash](https://linuxhint.com/concat_string_bash/)