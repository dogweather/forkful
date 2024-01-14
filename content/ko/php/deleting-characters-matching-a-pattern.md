---
title:    "PHP: 패턴과 일치하는 문자 삭제"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜
문자열에서 특정 패턴과 일치하는 문자를 삭제하는 작업은 프로그래밍에서 자주 사용되는 기술입니다. 이 글에서는 이 작업을 하는 이유를 소개하겠습니다.

## 어떻게
우선, 지우고싶은 패턴을 정규표현식으로 표현해야 합니다. 예를 들어, 다음과 같은 문자열이 있다고 가정해봅시다.

```PHP
$string = "Programming is fun, but regex can be tricky!";
```

만약 `regex`와 일치하는 문자를 모두 지우고 싶다면 다음과 같이 코드를 작성할 수 있습니다.

```PHP
$new_string = preg_replace('/regex/', '', $string);
```

결과는 다음과 같을 것입니다.

```PHP
"Programming is fun, but can be tricky!";
```

만약 `regex`와 `but`을 지우고 싶다면 다음과 같이 코드를 작성하면 됩니다.

```PHP
$new_string = preg_replace('/(regex|but)/', '', $string);
```

결과는 다음과 같을 것입니다.

```PHP
"Programming is fun, can tricky!";
```

## 더 깊은 내용
정규표현식을 사용하면 매우 유용한 문자열 작업을 할 수 있습니다. 하지만 정규표현식을 제대로 이해하지 못하면 예기치 않은 결과를 얻을 수도 있습니다. 그래서 정규표현식을 사용할 때 반드시 주의해야 합니다. 또한, PHP에서 제공하는 `preg_replace()` 함수의 다양한 옵션과 활용 방법도 알고 있어야 합니다.

## 또 보기
- [정규표현식 소개 - 생활코딩](https://opentutorials.org/module/1650/10205)
- [PHP preg_replace() 함수 - w3schools](https://www.w3schools.com/php/func_regex_preg_replace.asp)
- [정규표현식 연습 사이트 - regex101](https://regex101.com/)