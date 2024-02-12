---
title:                "인터랙티브 셸 (REPL) 사용하기"
aliases:
- ko/php/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:28.633424-07:00
model:                 gpt-4-0125-preview
simple_title:         "인터랙티브 셸 (REPL) 사용하기"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
인터랙티브 쉘 또는 REPL(Read-Eval-Print Loop)을 사용하면 PHP 코드를 즉석에서 작성하고 실행할 수 있습니다. 실험, 디버깅 또는 학습에 이상적이며, 스크립트를 전체적으로 생성하는 부담 없이 코드 조각을 테스트할 수 있습니다.

## 사용 방법:
터미널에서 `php -a`를 실행하여 PHP REPL을 시작합니다. 그것이 어떻게 작동하는지의 예는 다음과 같습니다:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
배열
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

함수를 정의할 수도 있습니다:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## 심층 분석
REPL은 1960년대 LISP 초기부터 어느 정도 형태로 존재해 왔습니다. PHP의 인터랙티브 쉘은 Python이나 JavaScript 같은 언어의 것에 비해 발전된 것이 적습니다. 세션 간 상태를 유지하지 않으며 자동 완성과 같은 기능이 부족합니다. 더 기능이 풍부한 PHP REPL을 원한다면, `psysh` 또는 `boris`와 같은 대안을 고려하세요. 이러한 타사 쉘은 더 나은 내부 조사 도구, 탭 완성 및 심지어 디버거까지 제공합니다.

내부적으로, PHP의 REPL은 입력된 각 코드 줄을 컴파일하고 실행함으로써 작동합니다. 같은 세션 내에서 클래스를 재선언하는 것과 같은 이 접근 방식의 한계점이 명확해집니다. 단순한 테스트에는 좋지만 복잡한 작업에는 번거로울 수 있습니다.

## 참고자료
- [PHP 매뉴얼 - 인터랙티브 쉘](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: PHP를 위한 런타임 개발자 콘솔, 인터랙티브 디버거 및 REPL](https://psysh.org/)
- [Boris: PHP를 위한 작은 REPL](https://github.com/borisrepl/boris)
