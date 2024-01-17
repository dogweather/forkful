---
title:                "'csv 파일 작업하기'"
html_title:           "PHP: 'csv 파일 작업하기'"
simple_title:         "'csv 파일 작업하기'"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-csv.md"
---

{{< edit_this_page >}}

파일을 읽거나 쓸 때 많은 양의 데이터를 다루는 것은 어려울 수 있습니다. 특히 엑셀과 같은 스프레드시트 프로그램의 형식을 사용할 때는 더 그렇습니다. 이런 경우에는 CSV(comma-separated values) 파일을 사용하는 것이 훨씬 효율적입니다. CSV 파일은 콤마로 구분된 데이터를 담고 있는 텍스트 형식의 파일입니다. 프로그래머들은 작업을 CSV 파일과 함께 하는 이유는 더 많은 데이터를 처리하고 조작할 수 있기 때문입니다.

## 무엇 & 왜?

CSV 파일은 쉽게 읽을 수 있고, 엑셀과 같은 프로그램에서도 쉽게 열 수 있습니다. 또한, 다른 데이터 포맷보다 더 적은 공간을 차지하며, 더 많은 데이터를 처리할 수 있습니다. 그래서 프로그래머들은 CSV 파일을 사용하여 큰 양의 데이터를 다루는 것에 유용합니다.

## 어떻게:

```PHP
// 새로운 CSV 파일 만들기
$csvFile = fopen('new_file.csv', 'w');

// 행 추가하기
fputcsv($csvFile, ['이름', '나이', '직업']);

// 데이터 추가하기
fputcsv($csvFile, ['John', '25', '개발자']);
fputcsv($csvFile, ['Jane', '28', '디자이너']);

// 파일 닫기
fclose($csvFile);

// CSV 파일 읽기
$csvFile = fopen('new_file.csv', 'r');

// while 루프를 사용하여 데이터 출력하기
while ($data = fgetcsv($csvFile)) {
    echo $data[0] . '의 나이는 ' . $data[1] . '이며, 직업은 ' . $data[2] . '입니다.' . "\n";
}

// 파일 닫기
fclose($csvFile);
```

출력:

```
이름의 나이는 나이이며, 직업은 직업입니다.
John의 나이는 25이며, 직업은 개발자입니다.
Jane의 나이는 28이며, 직업은 디자이너입니다.
```

## 깊은 수준:

CSV 파일은 엑셀 이전에도 이미 존재했으며, 오래된 데이터 포맷 중 하나입니다. 그리고 여전히 널리 사용되고 있습니다. CSV 파일을 다루는 또 다른 방법으로는 외부 라이브러리를 사용하는 것이 있습니다. 하지만 PHP는 기본적으로 fopen() 및 fputcsv() 함수를 제공하여 간단하게 CSV 파일을 다룰 수 있게 지원하고 있습니다.

## 같이 보기:

- PHP 공식 문서: http://php.net/manual/en/function.fgetcsv.php
- CSV 파일 포맷에 대한 더 많은 정보: https://en.wikipedia.org/wiki/Comma-separated_values