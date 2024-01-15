---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "PHP: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것은 프로그램에서 필요한 작업을 수행하기 전에 중요한 검사입니다. 디렉토리가 존재하는지 확인하는 것은 파일을 저장하거나 불러오는 작업을 할 때, 그리고 프로그램이 원활하게 작동하기 위해 필요한 여러 가지 작업을 수행할 때 중요합니다.

## 어떻게

```PHP
// 디렉토리 경로 지정
$dir = "/var/www/html";

// 디렉토리가 존재하는지 여부를 확인하는 함수
if (is_dir($dir)) {
  echo "해당 디렉토리가 존재합니다.";
} else {
  echo "해당 디렉토리가 존재하지 않습니다.";
}
```

위의 예시는 PHP의 `is_dir()` 함수를 사용하여 디렉토리가 존재하는지 확인하는 방법을 보여줍니다. 이 함수는 파라미터로 디렉토리의 경로를 받아서 해당 디렉토리가 존재하면 `true`를, 존재하지 않으면 `false`를 반환합니다. 

```PHP
// 디렉토리 경로 지정
$dir = "/var/www/html";

// 디렉토리의 정보를 얻는 함수
$dir_info = stat($dir);

// 디렉토리가 존재하는지 확인하는 조건문
if ($dir_info) {
  echo "해당 디렉토리가 존재합니다.";
} else {
  echo "해당 디렉토리가 존재하지 않습니다.";
}
```

또 다른 방법으로 `stat()` 함수를 사용하여 디렉토리의 정보를 얻은 뒤, 해당 정보를 이용하여 디렉토리가 존재하는지를 확인할 수 있습니다. `stat()` 함수는 파일이나 디렉토리의 상세한 정보를 배열의 형태로 반환해주기 때문에 디렉토리가 존재하면 배열의 값이 존재하고, 존재하지 않으면 `false`를 반환하게 됩니다.

## 딥 다이브

PHP에서 디렉토리가 존재하는지를 확인하기 위해 사용할 수 있는 함수는 많이 있습니다. `is_dir()` 함수 외에도 `file_exists()` 함수, `is_readable()` 함수, `glob()` 함수 등 다양한 함수를 사용할 수 있지만, 이들 함수는 각각 다른 목적에 맞게 사용되므로 신중하게 선택해야 합니다.

아래는 `glob()` 함수를 사용하여 디렉토리의 파일 리스트를 출력하는 예시입니다.

```PHP
// 디렉토리 경로 지정
$dir = "/var/www/html";

// 해당 디렉토리의 모든 파일 리스트를 배열로 반환하는 함수
$files = glob($dir . "/*");

// 파일 리스트를 출력하는 반복문
foreach($files as $file){
  echo "파일명: " . $file . "\n";
}
```

`glob()` 함수는 디렉토리 내부의 모든 파일 리스트를 배열로 반환하는 함수로, 위의 예시에서는 배열의 각 요소를 반복문으로 출력하는 방식으로 사용하였습니다.

## 관련 링크 

- [PHP 공식 사이트](https://www.php.net/)
- [PHP 디렉토리 관련 함수 안내](https://www.php.net/manual/en/ref.dir.php)
- [PHP 파일 관련 함수 안내](https://www.php.net/manual/en/ref.filesystem.php)