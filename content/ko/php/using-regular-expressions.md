---
title:    "PHP: 정규식을 사용하는 방법"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용해야 할까요?

정규 표현식은 PHP 프로그래밍에서 매우 유용한 도구입니다. 이를 사용하면 텍스트에서 원하는 부분을 정확하게 추출하거나 패턴을 대체할 수 있습니다. 또한 정규 표현식을 이용하면 더 간결하고 효율적인 코드를 작성할 수 있습니다. 

# 어떻게 사용할까요?

```PHP
// 텍스트에서 이메일 주소 추출하기
$text = "안녕하세요, 제 이메일 주소는 example@gmail.com입니다.";
preg_match("/[\w]+@[\w]+\.[\w]+/", $text, $matches);
echo $matches[0];
```

위 예제 코드를 실행하면 "example@gmail.com"이라는 이메일 주소가 출력될 것입니다. 이렇게 정규 표현식을 사용하면 쉽게 원하는 패턴의 텍스트를 추출할 수 있습니다.

# 깊이 파헤쳐보기

정규 표현식은 기본적으로 패턴을 나타내는 문자열이고, 이를 이용해 텍스트를 검색하고 추출하는 작업을 할 수 있습니다. 예를 들어 "/"로 시작하고 끝나는 문자열은 정규 표현식의 시작과 끝을 나타내며, 그 사이에 원하는 패턴을 작성할 수 있습니다. 그리고 패턴의 일부분을 `[]`로 감싸면 해당 부분의 문자들 중 하나와 매치됨을 의미합니다. `+`는 앞의 패턴이 하나 이상 있을 수 있음을 나타내고, `\w`는 문자나 숫자를 의미합니다.

정규 표현식을 더 학습하고 싶다면 [PHP 공식 문서](https://www.php.net/manual/en/book.pcre.php)를 참고해보세요.

# 더 알아보기

##- [Regular Expression Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
##- [Regex101 프로페셔널 가이드](https://www.regex101.com/guide/introduction)
##- [정규 표현식 연습 사이트 (exercism.io)](https://exercism.io/tracks/php/exercises/regex)

# 관련 링크

##- [PHP와 함께 정규 표현식 사용하기](https://www.php.net/manual/en/book.pcre.php)
##- [정규 표현식을 사용한 문자열 검색 및 대체](https://www.tutorialspoint.com/php/php_regular_expression.htm)
##- [더 많은 예제와 설명이 있는 Regular Expressions Cookbook 책](http://shop.oreilly.com/product/0636920012337.do)