---
title:    "PHP: 디렉토리가 존재하는지 확인하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
디렉토리가 존재하는지 확인하는 것이 중요한 이유는 파일이나 폴더를 생성하기 전에 해당 디렉토리가 존재하는지 확인하여 오류를 방지할 수 있기 때문입니다.

## 방법
다음은 PHP에서 디렉토리가 존재하는지 확인하는 방법입니다.
```PHP
if(file_exists($path)){ 
    echo "디렉토리가 존재합니다."; 
} else {
    echo "디렉토리가 존재하지 않습니다."; 
}
```
위 코드는 `$path` 변수에 저장된 디렉토리가 존재할 경우 "디렉토리가 존재합니다."를 출력하고, 그렇지 않을 경우 "디렉토리가 존재하지 않습니다."를 출력합니다.

## 더 깊이
PHP에서 디렉토리가 존재하는지 확인하는 방법은 `file_exists()` 함수 외에도 `is_dir()` 함수를 사용할 수도 있습니다. 이 함수는 파일이 아닌 디렉토리인지를 체크하여 `true` 혹은 `false` 값을 반환합니다.
또한, 디렉토리가 존재하지 않을 경우 디렉토리를 생성하는 방법도 함께 알아볼 수 있습니다. `mkdir()` 함수를 사용하여 새로운 디렉토리를 생성할 수 있으며, 이미 디렉토리가 존재하는 경우 `mkdir()` 함수는 `false` 값을 반환합니다.

## 더 보기
- [PHP 공식 문서 - file_exists()](https://www.php.net/manual/en/function.file-exists.php)
- [PHP 공식 문서 - is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [PHP 공식 문서 - mkdir()](https://www.php.net/manual/en/function.mkdir.php)