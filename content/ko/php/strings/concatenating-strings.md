---
date: 2024-01-20 17:35:36.432972-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) \uCC98\uC74C PHP\uAC00 \uB4F1\uC7A5\uD588\
  \uC744 \uB54C, \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uB370\uC774\uD0C0\uB97C \uC870\
  \uC791\uD558\uB294 \uD575\uC2EC \uAE30\uB2A5 \uC911 \uD558\uB098\uC600\uC2B5\uB2C8\
  \uB2E4. PHP\uB294 \uB2E8\uC21C\uD568\uACFC \uC720\uC5F0\uC131\uC744 \uC911\uC694\
  \uD558\uAC8C \uC5EC\uAE30\uBA70, \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uC774\uB97C\
  \ \uC798 \uBC18\uC601\uD569\uB2C8\uB2E4. \uD569\uCE58\uACE0 \uC2F6\uC740 \uBB38\uC790\
  \uC5F4\uC774 \uB9CE\uC744 \uACBD\uC6B0, \uC810('.') \uC5F0\uC0B0\uC790\uB97C \uBC18\
  \uBCF5\uC801\uC73C\uB85C \uC0AC\uC6A9\uD558\uAC70\uB098\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.048232-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C:) \uCC98\uC74C PHP\uAC00 \uB4F1\uC7A5\uD588\uC744 \uB54C\
  , \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 \uB370\uC774\uD0C0\uB97C \uC870\uC791\uD558\
  \uB294 \uD575\uC2EC \uAE30\uB2A5 \uC911 \uD558\uB098\uC600\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (어떻게:)
```PHP
<?php
// 문자열 연결
$greeting = "안녕하세요, ";
$name = "지은!";
$welcome = $greeting . $name;

echo $welcome; // 출력: 안녕하세요, 지은!
?>
```
```PHP
<?php
// '.' 연산자을 사용하는 또 다른 예
$part1 = "PHP는 ";
$part2 = "정말 재밌어요!";
$phrase = $part1 . $part2;

echo $phrase; // 출력: PHP는 정말 재밌어요!
?>
```

```PHP
<?php
// 복합 연산자 '.='를 이용한 예
$message = "오늘 날씨는 ";
$message .= "맑습니다.";

echo $message; // 출력: 오늘 날씨는 맑습니다.
?>
```

## Deep Dive (심층 분석)
처음 PHP가 등장했을 때, 문자열 연결은 데이타를 조작하는 핵심 기능 중 하나였습니다. PHP는 단순함과 유연성을 중요하게 여기며, 문자열 연결은 이를 잘 반영합니다. 합치고 싶은 문자열이 많을 경우, 점('.') 연산자를 반복적으로 사용하거나 implode() 함수를 활용하여 배열의 요소를 합칠 수 있습니다. 

```PHP
<?php
// implode() 예제
$words = ['PHP', '로', '코딩', '하는', '재미'];
$sentence = implode(" ", $words);

echo $sentence; // 출력: PHP 로 코딩 하는 재미
?>
```

PHP 5.6 이후로, 프로그래머들은 변종 문자열 표기법(heredoc, nowdoc)를 사용할 수도 있습니다. 이 방법은 큰 문장이나 복잡한 데이터를 다루는 데 유용합니다. 지난 몇 년간의 변화에도 불구하고, 단순한 '.' 연산자는 여전히 문자열을 합치는 가장 기본적이고 빠른 방법이죠.

## See Also (관련 링크)
- PHP 공식 문서 문자열 핸들링: [https://www.php.net/manual/en/language.types.string.php](https://www.php.net/manual/en/language.types.string.php)
- PHP 문자열 함수 목록: [https://www.php.net/manual/en/ref.strings.php](https://www.php.net/manual/en/ref.strings.php)
- 문자열 연결에 대한 더 깊은 이해: [https://www.php.net/manual/en/language.operators.string.php](https://www.php.net/manual/en/language.operators.string.php)
