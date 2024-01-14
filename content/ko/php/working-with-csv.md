---
title:                "PHP: csv 파일 다루기"
simple_title:         "csv 파일 다루기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜 CSV 작업을 해야 할까요?

CSV(쉼표로 구분된 값)는 다양한 유형의 데이터를 저장하고 처리하는 데에 유용한 형식입니다. PHP 언어는 CSV 파일을 쉽게 읽고 쓸 수 있는 기능을 제공하며, 이를 통해 다양한 데이터 처리 작업을 수행할 수 있습니다.

## 방법: CSV 작업 예제 및 출력

```PHP 
<?php
  // CSV 파일 열기
  $file = fopen("data.csv","r");

  // CSV 파일 읽기
  while(!feof($file)) {
      $data = fgetcsv($file);

      // CSV 값 출력
      echo "Name: " . $data[0] . ", Age: " . $data[1] . ", City: " . $data[2] . "\n";
  }

  // CSV 파일 닫기
  fclose($file);
?>
```

**출력결과:**

Name: John, Age: 25, City: Seoul

Name: Jane, Age: 30, City: Busan

Name: Mike, Age: 35, City: Incheon


위의 예제에서는 `fgetcsv()` 함수를 사용해 CSV 파일을 한 줄씩 읽고, `echo`를 통해 데이터를 출력합니다. 이처럼 PHP를 사용하면 CSV 파일의 데이터를 쉽게 읽고 처리할 수 있습니다.

## 깊이있게 살펴보기

이제 좀더 깊이있게 CSV 파일 작업에 대해 알아보겠습니다. PHP에서 CSV 파일을 읽고 쓰는 방법은 다양합니다. `fgetcsv()` 함수 외에도 `fputcsv()` 함수를 사용해 새로운 CSV 파일을 생성할 수 있습니다. 또한 `str_getcsv()` 함수를 사용해 문자열로 된 CSV 데이터를 처리할 수 있습니다.

또한 PHP의 내장 함수를 사용하면 CSV 파일을 쉽게 필터링하거나 정렬할 수 있습니다. `array_filter()` 함수를 사용해 불필요한 데이터를 제거하거나, `sort()` 함수를 사용해 데이터를 정렬할 수도 있습니다. 이처럼 PHP에서는 다양한 내장 함수를 활용해 CSV 파일을 효율적으로 처리할 수 있습니다.

## See Also

* [PHP 공식 문서 - CSV 함수](https://www.php.net/manual/en/ref.csv.php) 
* [쉼표로 구분된 값 (CSV) 파일 파싱하기](https://code.tutsplus.com/ko/tutorials/how-to-parse-a-csv-file-in-php--cms-37000)
* [CSV 파일 다루기](https://www.geeksforgeeks.org/working-csv-files-php/)
* [CSV 파일 열기 및 쓰기](https://www.guru99.com/parsing-csv-files.html)