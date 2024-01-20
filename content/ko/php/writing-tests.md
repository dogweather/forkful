---
title:                "테스트 작성"
html_title:           "PHP: 테스트 작성"
simple_title:         "테스트 작성"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-tests.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

코드를 작성하는 프로그래머들은 코드를 작성하기 전에 테스트 케이스를 작성하는 것이 좋습니다. 이를 통해 코드의 오류를 미리 발견하고 수정할 수 있기 때문입니다.

## 방법:

```PHP
<?php
// 예시 함수
function add($num1, $num2) {
  return $num1 + $num2;
}

echo add(5, 10); // 출력 결과: 15
```

위 예제에서 볼 수 있듯이, 함수를 작성할 때에도 테스트 케이스를 작성하는 것이 좋습니다. 이를 통해 함수가 제대로 작동하는지 확인할 수 있고, 수정할 때에도 이전에 작성한 테스트 케이스를 바탕으로 오류를 최소화할 수 있습니다.

## 더 깊이 들어가기:

테스트 케이스를 작성하는 것은 프로그래머들 사이에서 오래 전부터 추천되어온 방법입니다. 하지만 요즘에는 자동화된 테스트 도구들이 많이 개발되어 사용되고 있습니다. 이 도구들을 사용하면 훨씬 더 쉽게 테스트 케이스를 작성하고 실행할 수 있습니다. 또한 테스트 주도 개발(TDD) 방법론이 등장하면서, 테스트 케이스를 먼저 작성하고 그에 맞는 코드를 작성하는 방식이 널리 사용되고 있습니다.

## 더 알아보기:

- [PHP의 테스트 케이스 작성 방법 가이드](https://www.phpunit.de/doc/phpunit-book.html)
- [자동화된 PHP 테스트 도구인 PHPUnit 공식 사이트](https://phpunit.de/)