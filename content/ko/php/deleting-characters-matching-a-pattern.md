---
title:                "PHP: 패턴과 일치하는 문자 삭제"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 왜 삭제 패턴과 일치하는 문자를 지우는 것일까요?

코드를 작성하다 보면, 때로는 특정한 패턴과 일치하는 문자를 삭제해야 할 때가 있습니다. 이는 예를 들어, 문자열에서 모든 공백을 제거하거나, 특정한 문자열 패턴을 제거하는 경우에 유용합니다. 이러한 작업은 가독성을 높이거나 데이터를 정리할 때 매우 유용합니다.

## 어떻게 삭제 패턴과 일치하는 문자를 지울 수 있을까요?

PHP에서는 정규 표현식을 사용하여 패턴과 일치하는 문자를 삭제할 수 있습니다. 아래의 예시 코드를 참고해주세요.

```PHP
$text = "Hello World!";
$pattern = "/o/";
$new_text = preg_replace($pattern, "", $text);

echo $new_text;
// 결과: Hell Wrld!
```

위의 예시 코드에서는 "o"라는 패턴과 일치하는 모든 문자를 삭제하여 "Hello World!"라는 문자열에서 "o"를 없앴습니다. 이를 응용하면 원하는 패턴을 지우는 것이 가능합니다.

## 깊이 살펴보기

PHP에서 정규 표현식으로 문자를 삭제하는 방법은 여러 가지가 있습니다. 위에서 소개한 `preg_replace()` 외에도 `str_replace()`를 사용하는 방법이 있습니다. 또한, 정규 표현식을 작성하는 방법에도 다양한 문법이 있으며, 조건문을 활용하여 원하는 패턴을 지우는 것도 가능합니다.

# 같이 보기

- [PHP 정규 표현식 문서](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
- [온라인 정규 표현식 테스트 사이트](https://regexr.com)
- [PHP String 함수 문서](https://www.php.net/manual/en/ref.strings.php)