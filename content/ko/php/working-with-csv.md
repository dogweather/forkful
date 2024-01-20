---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (CSV 작업의 이해와 필요성)
CSV(Comma-Separated Values) 파일은 간단한 텍스트로 데이터를 저장합니다. 프로그래머는 데이터 교환과 저장 효율성을 위해 CSV를 사용합니다.

## How to: (실제 사용법)
CSV 파일 읽기:
```PHP
<?php
$filename = 'data.csv';
if (($handle = fopen($filename, 'r')) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        print_r($data);
    }
    fclose($handle);
}
?>
```
출력 예시:
```
Array
(
    [0] => 이름
    [1] => 직업
    [2] => 연락처
)
...
```

CSV 파일 쓰기:
```PHP
<?php
$list = [
    ['홍길동', '개발자', '012-3456-7890'],
    ['이순신', '디자이너', '098-7654-3210']
];

$filename = 'output.csv';
$handle = fopen($filename, 'w');
foreach ($list as $fields) {
    fputcsv($handle, $fields);
}
fclose($handle);
?>
```

## Deep Dive (심화 탐구)
- 초기 컴퓨터 시대부터 데이터 교환 형식으로 CSV가 사용되었습니다.
- JSON, XML 같은 현대적 데이터 포맷과 비교하며 CSV는 여전히 단순한 데이터 이동에 최적화되어 있습니다.
- PHP에서 `fgetcsv`와 `fputcsv` 함수는 CSV 파일과의 데이터 입출력을 처리합니다. 각각 읽기와 쓰기 작업에 사용됩니다.

## See Also (참고자료)
- PHP Manual on fgetcsv: [https://www.php.net/manual/en/function.fgetcsv.php](https://www.php.net/manual/en/function.fgetcsv.php)
- PHP Manual on fputcsv: [https://www.php.net/manual/en/function.fputcsv.php](https://www.php.net/manual/en/function.fputcsv.php)
- RFC 4180, Common Format and MIME Type for Comma-Separated Values (CSV) Files: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)