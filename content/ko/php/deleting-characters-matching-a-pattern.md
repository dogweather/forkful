---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "PHP: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜 해야 할까요? 

패턴과 일치하는 문자를 삭제하는 것은 데이터 정리나 특정 조건에 맞는 문자를 찾는데 유용합니다. 또한 코드를 간단하게 유지하고 가독성을 높일 수 있습니다.

## 어떻게 하면 될까요? 

```
PHP에서는 preg_replace() 함수를 사용하여 문자열에서 패턴에 해당하는 문자를 삭제할 수 있습니다.

<?php
$original_string = "Hello, World!";
$pattern = "/l+/";
$replacement = "";
$new_string = preg_replace($pattern, $replacement, $original_string);
echo $new_string; // 출력 결과 : Heo, Word!
?>

우리는 문자열 "Hello, World!"에서 "l"이라는 문자가 매칭되는 패턴을 찾아 해당 문자를 빈 문자열로 대체하여 새로운 문자열을 만들었습니다. 따라서 "Hello, World!"에서 "l"이라는 문자를 모두 삭제하였습니다.
```

## 더 깊이 들어가보기 

문자열에서 패턴에 해당하는 문자를 삭제하기 위해서는 정규표현식을 사용해야 합니다. 정규표현식은 문자열에서 패턴을 검색할 수 있도록 도와주는 매우 유용한 도구입니다. 

위의 예제에서 사용한 "/l+/"는 정규표현식의 일부로, "l"이라는 문자가 1개 이상 연속으로 나오는 패턴을 의미합니다. 이 외에도 다양한 정규표현식을 사용하여 원하는 문자를 찾고 삭제할 수 있습니다.

## 더 알아보기 

- [PHP: preg_replace() 함수 문서](https://www.php.net/manual/ko/function.preg-replace.php)
- [정규표현식 테스트 사이트](https://regexr.com/)
- [레퍼런스 사이트에서 더 많은 정규표현식 패턴 확인하기](https://www.php.net/manual/ko/reference.pcre.pattern.syntax.php)

## 참고 자료 

다양한 패턴에 대한 정규표현식을 사용하여 문자를 삭제하고 더 많은 기능을 구현하실 수 있습니다. 부수적인 코드를 최소화하고 가독성을 높이기 위해 가능한 경우 정규표현식을 사용해보시길 권장합니다.

See Also: [PHP에서 문자열의 문자 삭제하기](https://webruden.tistory.com/)