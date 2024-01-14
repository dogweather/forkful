---
title:                "PHP: 디렉토리가 존재하는지 확인하는 방법"
simple_title:         "디렉토리가 존재하는지 확인하는 방법"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜 

디렉토리가 존재하는지 확인하는 것의 이유는 무엇일까요? 간단히 말씀드리자면, 프로그래밍 중에 당신은 파일 시스템 내부에서 작업하는 경우가 많습니다. 디렉토리의 존재 유무를 확인하지 않으면, 예상치 못한 오류가 발생할 수 있습니다. 

## 어떻게

우선 디렉토리가 존재하는지 확인하는 가장 간단한 방법은 `file_exists()` 함수를 사용하는 것입니다. 이 함수는 파일 혹은 디렉토리의 존재 유무를 판단하여 `true` 혹은 `false` 값을 리턴해줍니다. 아래의 예제 코드를 참고해주세요. 

```PHP
// 디렉토리 경로 설정
$directory = "/home/user/uploads";

// 디렉토리가 존재하는지 확인
if (file_exists($directory)) {
    echo "디렉토리가 존재합니다.";
} else {
    echo "디렉토리가 존재하지 않습니다.";
}

// 존재하는 디렉토리에서 파일 목록 출력
$files = scandir($directory);
foreach ($files as $file) {
    echo $file . "\n";
}
```

위의 코드를 실행하면, 디렉토리의 존재 유무를 확인하고 존재하는 경우 해당 디렉토리 내의 파일 목록을 출력하는 결과를 볼 수 있습니다. 

## 심층 탐구

`file_exists()` 함수 외에도, `is_dir()` 함수를 사용하여 디렉토리의 존재 유무를 확인할 수 있습니다. 이 함수는 해당 경로가 디렉토리인지 판별하여 `true` 혹은 `false` 값을 리턴해줍니다. 또한, `scandir()` 함수를 사용하여 디렉토리 내의 파일 목록을 배열로 리턴받을 수도 있습니다. 

또 한 가지 중요한 포인트는, 디렉토리의 존재 유무를 확인하기 전에 해당 디렉토리가 어떤 권한을 가지고 있는지 확인하는 것입니다. 만약 쓰기 권한이 없는 디렉토리에 새로운 파일을 생성하려 하면, 오류가 발생할 수 있습니다. 따라서 디렉토리의 권한을 먼저 확인하고, 필요한 권한을 설정해주는 것이 안전한 방법입니다. 

## 더 알아보기 

[PHP 공식 문서 - file_exists()](https://www.php.net/manual/kr/function.file-exists.php) 

[PHP 공식 문서 - is_dir()](https://www.php.net/manual/kr/function.is-dir.php) 

[PHP 공식 문서 - scandir()](https://www.php.net/manual/kr/function.scandir.php) 

See Also 

[PHP로 파일 및 디렉토리 생성하기](https://www.example.com) 

[디렉토리와 파일 시스템 - PHP: 파일 디렉토리 관리](https://www.example.com) 

[PHP로 파일 업로드하기](https://www.example.com)