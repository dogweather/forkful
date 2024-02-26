---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:42.309578-07:00
description: "CSV(\uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12) \uC791\uC5C5\uC740\
  \ \uD3C9\uBB38\uC73C\uB85C \uD45C \uD615\uC2DD \uB370\uC774\uD130\uB97C \uB098\uD0C0\
  \uB0B4\uB294 \uC778\uAE30 \uC788\uB294 \uD615\uC2DD\uC778 CSV \uD30C\uC77C\uC5D0\
  \uC11C \uB370\uC774\uD130\uB97C \uC77D\uACE0 \uC4F0\uB294 \uAC83\uC744 \uD3EC\uD568\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uB2E4\uC591\uD55C\
  \ \uD504\uB85C\uADF8\uB7A8, \uC2DC\uC2A4\uD15C \uB610\uB294 \uB370\uC774\uD130\uBCA0\
  \uC774\uC2A4 \uAC04\uC5D0 \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uAD50\uD658\uD558\
  \uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\uB370, \uC774\
  \uB294 \uADF8 \uB2E8\uC21C\uC131\uACFC\u2026"
lastmod: '2024-02-25T18:49:52.384434-07:00'
model: gpt-4-0125-preview
summary: "CSV(\uC27C\uD45C\uB85C \uAD6C\uBD84\uB41C \uAC12) \uC791\uC5C5\uC740 \uD3C9\
  \uBB38\uC73C\uB85C \uD45C \uD615\uC2DD \uB370\uC774\uD130\uB97C \uB098\uD0C0\uB0B4\
  \uB294 \uC778\uAE30 \uC788\uB294 \uD615\uC2DD\uC778 CSV \uD30C\uC77C\uC5D0\uC11C\
  \ \uB370\uC774\uD130\uB97C \uC77D\uACE0 \uC4F0\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uB2E4\uC591\uD55C \uD504\
  \uB85C\uADF8\uB7A8, \uC2DC\uC2A4\uD15C \uB610\uB294 \uB370\uC774\uD130\uBCA0\uC774\
  \uC2A4 \uAC04\uC5D0 \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uAD50\uD658\uD558\uAE30\
  \ \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD558\uB294\uB370, \uC774\uB294\
  \ \uADF8 \uB2E8\uC21C\uC131\uACFC\u2026"
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

CSV(쉼표로 구분된 값) 작업은 평문으로 표 형식 데이터를 나타내는 인기 있는 형식인 CSV 파일에서 데이터를 읽고 쓰는 것을 포함합니다. 프로그래머들이 다양한 프로그램, 시스템 또는 데이터베이스 간에 데이터를 쉽게 교환하기 위해 이 작업을 수행하는데, 이는 그 단순성과 플랫폼 및 프로그래밍 언어 전반에 걸친 넓은 지원 덕분입니다.

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
