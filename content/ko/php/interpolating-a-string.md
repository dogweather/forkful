---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그렇게 하는가?

문자열 보간은 변수들로 문자열 안에 문법을 직접 "삽입"하는 것을 의미합니다. 이를 사용하는 주요 이유는 코드를 간결하게 유지하고, 문자열 복합을 훨씬 더 쉽게 만들기 위해서입니다.

## 어떻게 하는가?

아래는 PHP에서 문자열 보간을 하는 방법에 대한 간단한 예입니다.

```PHP
$name = "John";
echo "Hello, $name!";
```

이 코드를 실행하면, 결과는 다음과 같습니다:

```
Hello, John!
```

'$', 이 변수 식별자를 사용하면, 문자열 내에서 변수를 참조할 수 있습니다.

## 깊이 알아보기

문자열 보간은 긴 역사를 가진 프로그래밍 기법으로, 많은 언어에서 활용되고 있습니다. PHP에서도 적극 활용되며, 기존에 사용되던 문자열 결합 방식보다 훨씬 간결하고 직관적이라는 장점이 있습니다. 

하지만 문자열 보간 방식에도 다른 대안이 있습니다. 

```PHP
$name = "John";
echo "Hello, " . $name . "!";
```

위 코드는 문자열 결합을 이용해 동일한 결과를 만들어냅니다. 

그러나 문자열 보간이 더욱 명확하고 간결하게 코드를 작성할 수 있게 해주므로 권장됩니다.

## 참고 자료

더 자세한 정보와 추가적인 종류에 대해서는 아래 링크에서 확인하실 수 있습니다.

1. [PHP String Interpolation](https://www.w3schools.com/php/php_string.asp)
2. [PHP String Concatenation](https://www.php.net/manual/en/language.operators.string.php)

다음에도 좋은 정보로 찾아뵙겠습니다.