---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 쓰나요?

디렉토리가 존재하는지 확인하는 것은 파일 시스템에서 특정 디렉토리가 있는지를 검증하는 것입니다. 가끔씩 데이터를 읽거나 저장해야 할 때, 해당 디렉토리가 없으면 에러가 발생할 수 있기 때문에, 이런 과정이 중요합니다. 

## 어떻게 하나요:

PHP에서 디렉토리가 존재하는지 확인하려면 `is_dir` 함수를 사용합니다. 

```PHP
<?php
$dir = "/path/to/your/directory";

if(is_dir($dir)){
    echo "The directory exists";
}else{
    echo "The directory does not exist";
}
?>
```

위의 코드를 실행하면, 해당 디렉토리가 존재하는지에 따라 "The directory exists" 또는 "The directory does not exist"라는 메시지가 출력됩니다.

## Deep Dive

디렉토리의 존재 여부를 확인하는 것은 PHP의 초기 버전부터 필요했던 요소입니다. 파일 I/O 작업이 중요한 작업이기 때문입니다. 

`is_dir` 함수를 사용하는 것 외에도 `file_exists` 함수를 사용해도 디렉토리의 존재 여부를 확인할 수 있습니다. 하지만 `file_exists` 함수는 파일 뿐만 아니라 디렉토리의 존재 여부도 확인하기 때문에 사용에 주의가 필요합니다.

```PHP
<?php
$dir = "/path/to/your/directory";

if(file_exists($dir)){
    echo "The directory or file exists";
}else{
    echo "The directory or file does not exist";
}
?>
```

`is_dir` 함수는 내부적으로 `stat` 시스템 호출을 이용해 디렉토리 정보를 얻어와서 디렉토리인지 아닌지를 판별합니다.

## 참고 자료

1. `is_dir` 함수에 대한 PHP 공식 문서: [Link](https://www.php.net/manual/en/function.is-dir.php)
2. `file_exists` 함수에 대한 PHP 공식 문서: [Link](https://www.php.net/manual/en/function.file-exists.php)
3. `stat` 시스템 호출에 대한 정보: [Link](https://man7.org/linux/man-pages/man2/stat.2.html)