---
title:                "CSV 파일 작업하기"
html_title:           "PHP: CSV 파일 작업하기"
simple_title:         "CSV 파일 작업하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜
CSV 파일은 데이터를 쉽게 저장하고 전송할 수 있는 형식입니다. 그래서 PHP에서도 자주 사용됩니다.

## 작동 방법
```PHP
// CSV 파일 열기
$file = fopen('data.csv', 'r');

// 각 라인 순회
while (($line = fgetcsv($file)) !== FALSE) {
  // 각 라인에 있는 데이터 순회
  foreach ($line as $data) {
    // 데이터 출력
    echo $data;
  }
  echo "\n"; // 다음 라인으로 넘어가기 위해 개행문자 출력
}

// 파일 닫기
fclose($file);
```

위의 예시 코드는 CSV 파일을 열고 데이터를 라인 단위로 순회하며 출력하는 간단한 방법을 보여줍니다. 이와 같은 방법으로 데이터를 읽어오거나 쓰는 것이 가능합니다.

## 깊이있게 알아보기
CSV 파일을 다루기 위해서는 몇 가지 주의해야 할 점이 있습니다. 첫째, 파일을 열 때 적절한 인코딩을 설정해야 합니다. 일반적으로 utf-8 혹은 ASCII 인코딩을 사용하며, 다른 인코딩을 사용할 경우 해당 인코딩을 명시적으로 지정해 주어야 합니다. 둘째, 각 라인마다 콤마(,)로 구분되는 데이터를 포함하고 있기 때문에, PHP의 내장함수인 `str_getcsv()`를 사용하면 더 편리하게 데이터를 읽어올 수 있습니다. 마지막으로, CSV 파일을 수정할 때 기존 데이터의 형식을 유지하기 위해 `fputcsv()`를 사용하면 좋습니다.

## 더 알아보기
- [PHP의 CSV 관련 함수](https://www.php.net/manual/en/ref.csv.php)
- [CSV 파일 만들기](https://www.w3schools.com/php/php_file_create.asp)
- [CSV 파일 읽기](https://www.w3schools.com/php/php_file_open.asp)

## 참고
- [Markdown 문법 가이드](https://www.markdownguide.org/basic-syntax/)