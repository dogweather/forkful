---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:42.309578-07:00
description: "\uBC29\uBC95: PHP\uB294 CSV \uD30C\uC77C\uC744 \uB2E4\uB8E8\uAE30 \uC704\
  \uD55C \uB0B4\uC7A5 \uD568\uC218\uB97C \uC81C\uACF5\uD558\uC5EC, \uC81C3\uC790 \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uAC00 \uD544\uC694 \uC5C6\uC774 \uC774\uB7EC\uD55C \uD30C\
  \uC77C\uC744 \uC77D\uACE0 \uC4F0\uB294 \uAC83\uC744 \uAC04\uB2E8\uD558\uAC8C \uB9CC\
  \uB4E4\uC5B4\uC90D\uB2C8\uB2E4. \uC2DC\uC791\uD558\uAE30 \uC704\uD55C \uC608\uC81C\
  \uB4E4\uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\uB2C8\uB2E4: `fopen()`\uACFC \uD568\
  \uAED8 `fgetcsv()`\uB97C \uC0AC\uC6A9\uD558\uC5EC CSV \uD30C\uC77C\uC744 \uC5F4\uACE0\
  \ \uADF8\u2026"
lastmod: '2024-03-13T22:44:55.389839-06:00'
model: gpt-4-0125-preview
summary: "PHP\uB294 CSV \uD30C\uC77C\uC744 \uB2E4\uB8E8\uAE30 \uC704\uD55C \uB0B4\uC7A5\
  \ \uD568\uC218\uB97C \uC81C\uACF5\uD558\uC5EC, \uC81C3\uC790 \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uAC00 \uD544\uC694 \uC5C6\uC774 \uC774\uB7EC\uD55C \uD30C\uC77C\uC744 \uC77D\
  \uACE0 \uC4F0\uB294 \uAC83\uC744 \uAC04\uB2E8\uD558\uAC8C \uB9CC\uB4E4\uC5B4\uC90D\
  \uB2C8\uB2E4."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

## 방법:
PHP는 CSV 파일을 다루기 위한 내장 함수를 제공하여, 제3자 라이브러리가 필요 없이 이러한 파일을 읽고 쓰는 것을 간단하게 만들어줍니다. 시작하기 위한 예제들은 다음과 같습니다:

### CSV 파일 읽기
`fopen()`과 함께 `fgetcsv()`를 사용하여 CSV 파일을 열고 그 내용을 읽을 수 있습니다:

```php
<?php
$filename = 'data.csv';
$handle = fopen($filename, "r");
if ($handle !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        $num = count($data);
        echo "라인의 필드 수: $num\n";
        for ($c = 0; $c < $num; $c++) {
            echo $data[$c] . "\n";
        }
    }
    fclose($handle);
}
?>
```

이 스크립트는 각 라인의 필드 수와 각 필드의 내용을 출력합니다.

### CSV 파일 쓰기
`fopen()`을 쓰기 모드(`w`)로 사용하고, `fputcsv()`을 사용하여 CSV 파일에 쓸 수 있습니다:

```php
<?php
$list = [
    ['ID', '이름', '이메일'],
    [1, 'John Doe', 'john@example.com'],
    [2, 'Jane Doe', 'jane@example.com']
];

$handle = fopen('users.csv', 'w');

foreach ($list as $row) {
    fputcsv($handle, $row);
}

fclose($handle);
?>
```

이 스크립트는 `users.csv`라는 파일을 생성하고 헤더와 두 개의 데이터 행을 그 안에 씁니다.

### 라이브러리 사용하기: League\Csv
보다 고급 CSV 처리를 위해서, `League\Csv` 라이브러리는 강력한 기능 세트를 제공합니다. Composer를 통해 설치한 후 (`composer require league/csv`), CSV 데이터를 더 유연하게 읽고 쓸 수 있습니다.

#### League\Csv로 읽기
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Reader;

$csv = Reader::createFromPath('data.csv', 'r');
$csv->setHeaderOffset(0); // 첫 번째 행을 헤더로 사용하려면 설정

$results = $csv->getRecords();
foreach ($results as $row) {
    print_r($row);
}
?>
```

이 스크립트는 `data.csv`를 읽고, 첫 번째 행을 열 헤더로 취급하여 각 행을 연관 배열로 출력합니다.

#### League\Csv로 쓰기
```php
<?php
require 'vendor/autoload.php';

use League\Csv\Writer;

$csv = Writer::createFromPath('users_new.csv', 'w+');

$csv->insertOne(['ID', '이름', '이메일']);
$csv->insertAll([
    [3, 'Alex Doe', 'alex@example.com'],
    [4, 'Anna Smith', 'anna@example.com']
]);

echo "users_new.csv에 성공적으로 쓰였습니다.";
?>
```

이는 `users_new.csv`를 생성하고 헤더 행을 쓴 다음 두 데이터 행을 씁니다.
