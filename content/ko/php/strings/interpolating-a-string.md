---
date: 2024-01-20 17:51:41.683391-07:00
description: "How to: (\uBC29\uBC95) PHP\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\
  \uC744 \uC0AC\uC6A9\uD558\uB824\uBA74, \uC774\uC911 \uC778\uC6A9\uBD80\uD638(\"\
  )\uB85C \uBB36\uC778 \uBB38\uC790\uC5F4 \uC548\uC5D0 \uBCC0\uC218\uB97C \uB123\uAE30\
  \uB9CC \uD558\uBA74 \uB429\uB2C8\uB2E4. \uAC04\uB2E8\uD55C \uC608\uC81C\uB97C \uD1B5\
  \uD574 \uC54C\uC544\uBD05\uC2DC\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.041988-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) PHP\uC5D0\uC11C \uBB38\uC790\uC5F4 \uBCF4\uAC04\uC744 \uC0AC\
  \uC6A9\uD558\uB824\uBA74, \uC774\uC911 \uC778\uC6A9\uBD80\uD638(\")\uB85C \uBB36\
  \uC778 \uBB38\uC790\uC5F4 \uC548\uC5D0 \uBCC0\uC218\uB97C \uB123\uAE30\uB9CC \uD558\
  \uBA74 \uB429\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (방법)
PHP에서 문자열 보간을 사용하려면, 이중 인용부호(")로 묶인 문자열 안에 변수를 넣기만 하면 됩니다. 간단한 예제를 통해 알아봅시다.

```PHP
$name = "지훈";
$age = 28;

// 직접 변수를 문자열 안에 삽입
echo "안녕하세요, 제 이름은 $name이고, 나이는 $age살입니다.";

// 출력: 안녕하세요, 제 이름은 지훈이고, 나이는 28살입니다.

// 중괄호를 사용해 명확한 변수 경계를 지정
echo "안녕하세요, 제 이름은 {$name}이고, 나이는 {$age}살입니다.";
```
중괄호는 복잡한 표현식이나 배열 내부의 변수를 참조할 때 유용합니다.

```PHP
$info = ['name' => '지훈', 'age' => 28];

echo "{$info['name']}의 나이는 {$info['age']}살입니다.";

// 출력: 지훈의 나이는 28살입니다.
```

## Deep Dive (깊이있게 탐구하기)
PHP에서 문자열 보간은 PHP 4 시절부터 있던 기능입니다. 다른 언어에서도 보간을 허용하기는 하지만, PHP는 이중 인용부호와 중괄호를 사용해 유연한 방식으로 보간을 지원합니다. 이와 대조적으로, 단일 인용부호(') 안의 내용은 보간되지 않습니다.

문자열 결합을 위해 점(.) 연산자를 사용할 수도 있으나, 보간을 사용하면 더 읽기 쉽고 간결한 코드를 작성할 수 있습니다.

```PHP
// 문자열 결합을 사용한 경우
echo '안녕하세요, 제 이름은 ' . $name . '이고, 나이는 ' . $age . '살입니다.';
```
하지만, 보간을 사용할 때는 복잡한 표현식에 주의해야 하며, 보안 측면에서 사용자 입력을 직접 보간하지 말아야 합니다. 사용자 입력을 처리하는 경우에는 `htmlspecialchars()` 함수를 사용하여 XSS 공격을 방지할 수 있습니다.

## See Also (더보기)
- PHP 공식 문서의 문자열 보간: [https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)
- PHP에서 XSS 공격 방지하기: [https://www.php.net/manual/en/function.htmlspecialchars.php](https://www.php.net/manual/en/function.htmlspecialchars.php)
- 보다 복잡한 문자열 조작을 위한 sprintf 함수: [https://www.php.net/manual/en/function.sprintf.php](https://www.php.net/manual/en/function.sprintf.php)
