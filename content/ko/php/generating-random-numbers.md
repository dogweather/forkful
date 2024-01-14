---
title:    "PHP: 임의의 숫자 생성"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
난수 생성에 참여하는 이유는 다양할 수 있습니다. 예를 들어, 보안 관련 작업을 하거나 게임을 만들 때 사용할 수 있습니다. 또한 테스트나 평가를 위해 필요한 값들을 랜덤하게 가져오는데 활용할 수도 있습니다.

## 하는 법
PHP에서 난수를 생성하는 가장 기본적인 방법은 `rand()` 함수를 사용하는 것입니다. 아래는 몇 가지 예제 코드와 그에 대한 출력 결과입니다.

### 기본적인 난수 생성
```PHP
<?php
  $random_number = rand(); // 랜덤한 정수를 생성합니다.
  echo $random_number; // 예: 483666
?>
```

### 범위를 지정하여 난수 생성
```PHP
<?php
  $random_number = rand(1, 100); // 1부터 100 사이의 난수를 생성합니다.
  echo $random_number; // 예: 55
?>
```

### 부동소수점 수를 포함한 난수 생성
```PHP
<?php
  $random_number = rand(1, 100) / 10; // 1부터 100 사이의 난수를 생성하고 10으로 나누어 부동소수점 수를 포함합니다.
  echo $random_number; // 예: 8.9
?>
```

## 깊게 들어가기
PHP에서 난수를 생성하는 다양한 함수와 사용법을 알아보았습니다. 이 외에도 `mt_rand()`, `random_int()`, `random_bytes()` 함수 등을 사용하여 난수를 생성할 수 있습니다. 이 함수들은 보안적인 측면에서 더 좋은 결과를 보장해주기 때문에 보안 관련 작업에 활용하는 것이 좋습니다. 하지만 기본적인 난수 생성을 위해서는 `rand()` 함수를 사용하면 충분합니다.

## 또 다른 정보
- [PHP 공식 매뉴얼 - 랜덤 함수](https://www.php.net/manual/en/function.rand.php)
- [라라벨 랜덤 함수 가이드](https://laravel.com/docs/8.x/helpers#method-array-helpers)
- [보안을 위한 PHP 난수 생성 방법](https://www.php.net/manual/en/function.random-int.php)
- [난수 생성 방법 비교 및 성능 분석](https://www.php.net/manual/en/function.random-int.php)

## 참고
- [Markdown 가이드](https://www.markdownguide.org/)
  - [PHP에서 Markdown 사용하기](https://thephpdev.com/2019/03/29/how-to-render-markdown-files-using-php-erusev-parsedown/)
- [Markdown 에디터 - Typora](https://typora.io/)
- [IAMPARK - PHP 교육 강의 및 블로그](https://iampark.tistory.com/)