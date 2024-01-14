---
title:    "PHP: 디렉토리가 존재하는지 확인하기"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 왜

파일 시스템에는 디렉토리와 파일이 포함되어있습니다. 때때로 이들 디렉토리를 조작해야 할 때가 있을 겁니다. 그래서 디렉토리가 존재하는지 확인해야 합니다.

## 어떻게

```PHP
<?php
$dir = './existing_directory';
if (file_exists($dir) && is_dir($dir)) {
    echo "디렉토리가 존재합니다.";
} else {
    echo "디렉토리가 존재하지 않습니다.";
}
```

```PHP
<?php
$dir = './non_existent_directory';
if (file_exists($dir) && is_dir($dir)) {
    echo "디렉토리가 존재합니다.";
} else {
    echo "디렉토리가 존재하지 않습니다.";
}
```

위 코드에서는 두 가지 함수를 사용하여 디렉토리의 존재를 확인합니다. `file_exists()`는 해당 경로의 파일이나 디렉토리가 존재하는지를 확인하는 함수이고, `is_dir()`은 해당 경로가 디렉토리인지를 확인하는 함수입니다. 이 두 가지 함수 모두 boolean 값을 반환합니다. 따라서 위 코드에서는 `if`문을 사용하여 두 함수의 결과를 조합하여 디렉토리가 존재하는지를 확인한 후 상황에 맞게 출력해줍니다.

## 딥 다이브

때로는 `file_exists()` 함수만으로 디렉토리의 존재를 확인하는 것이 부족할 수 있습니다. 이는 디렉토리가 존재하지 않는다고 판단되는 경우에도 디렉토리가 실제로 존재할 수 있기 때문입니다. 이 경우에는 `realpath()` 함수를 사용하여 확인할 수 있습니다. `realpath()` 함수는 `file_exists()` 함수와 달리 실제로 해당 경로에 파일이나 디렉토리가 존재하는지를 확인합니다.

또한, 디렉토리가 존재하지 않는다는 것만으로는 문제를 해결하지 못할 수도 있습니다. 만약 디렉토리가 존재하지 않는다면 해당 디렉토리를 생성하는 코드를 추가해줘야 할 수도 있습니다. 따라서 디렉토리가 존재하는지를 확인할 때에는 상황에 따라 추가적인 처리가 필요할 수 있습니다.

## 또 다른 참고 자료

- [PHP 공식 문서 - file_exists() 함수](https://www.php.net/manual/ko/function.file-exists.php)
- [PHP 공식 문서 - is_dir() 함수](https://www.php.net/manual/ko/function.is-dir.php)
- [PHP 공식 문서 - realpath() 함수](https://www.php.net/manual/ko/function.realpath.php)