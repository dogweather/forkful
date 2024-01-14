---
title:    "PHP: 테스트 작성하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/writing-tests.md"
---

{{< edit_this_page >}}

## 왜?

오늘날 많은 프로그래밍 언어들은 자동화된 테스트가 가능하게끔 제공되고 있습니다. 이는 코드의 안전성과 신뢰성을 보장하기 위해 꼭 필요한 단계입니다. 테스트를 작성하는 것은 우리 개발자들이 품질 좋은 코드를 작성하는데 도움이 됩니다.

## 방법

테스트를 작성하는 첫 번째 단계는 테스트를 수행하기 위해 필요한 PHP 유닛 테스트 프레임워크인 PHPUnit을 설치하는 것입니다. 그 다음 다음과 같이 테스트를 작성할 수 있습니다.

```
<?php

// 예시 코드를 만들어봅시다
 function add($a, $b)
 {
     return $a + $b;
 }

// 테스트를 작성합니다
 class AddFunctionTest extends PHPUnit\Framework\TestCase
 {
     public function testAddTwoNumbers()
     {
        $result = add(3, 5);
        $this->assertEquals(8, $result);
     }
 }
```

위의 코드에서 우리는 `add()` 함수를 작성하고, `AddFunctionTest` 클래스를 만들어 `testAddTwoNumbers()` 메소드를 추가합니다. 이 함수 내에서 우리는 3과 5를 더한 뒤 결과가 8이 나오는지를 `assertEquals()` 함수를 통해 확인합니다.

테스트를 실행하기 위해 다음 명령어를 입력해봅시다.

```
phpunit AddFunctionTest.php
```

위의 예제는 간단한 예시이지만, 복잡한 코드를 테스트할 때에도 동일한 방법으로 진행됩니다.

## Deep Dive

테스트를 작성하는 것은 우리가 선발한 테스트 코드가 실제로 우리가 원하는 기능을 잘 수행하는지를 확인하기 위한 것입니다. 이는 우리가 코드를 수정하거나 새로운 기능을 추가할 경우에도 중요합니다. 테스트를 통해 우리는 변경된 코드가 예상대로 동작하는지를 확인할 수 있고, 오류를 빠르게 발견하여 수정할 수 있습니다.

또한 자동화된 테스트는 코드의 변화가 있을 때마다 매번 수동으로 테스트하는 것보다 더 적은 시간과 노력을 필요로 합니다. 이는 우리 개발자들의 생산성을 높여주는 중요한 요소입니다.

## 또 다른 방법들 알아보기

- [PHP 공식 문서 - PHPUnit 설치 가이드](https://phpunit.de/manual/8.3/ko/installation.html)
- [우아한 테크코스 - PHPUnit 사용방법](https://woowacourse.github.io/javable/post/2020-04-25-phpunit/)
- [PHP 서적 - 실습으로 배우는 PHP 프로그래밍](https://book.naver.com/bookdb/book_detail.nhn?bid=15358788)

## 더 많은 자료 검토하기

[더 많은 PHP 프로그래밍 관련 자료를 확인해보세요.](http://koreabase.net/sub5_2.php?no=310)