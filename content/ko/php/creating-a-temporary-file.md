---
title:                "PHP: 온소트 파일 만들기"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일 생성에 참여하는 *왜* 누군가에 대해 1-2 문장으로 설명합니다.

임시 파일을 생성하는 것은 PHP 프로그래밍에서 매우 유용합니다. 대개 이런 파일들은 일시적으로 사용되며 다른 프로그램이나 프로세스에서 읽거나 쓰기에 사용됩니다. 예를 들어, 파일을 다운로드하거나 데이터베이스를 업데이트하는 동안 매우 유용합니다.

## 사용 방법

PHP의 `tempnam()` 함수는 임시 파일을 생성하는 데 큰 도움이 됩니다. 이 함수는 첫 번째 매개 변수로 사용할 임시 파일 경로, 두 번째 매개 변수로 접두사 및 세 번째 매개 변수로 임시로 접근 가능한 디렉토리 위치를 사용합니다. 예를 들어:

```PHP
$temp_file = tempnam('/tmp', 'my_temp_file_'); // /tmp/my_temp_file_qsObqT
```

임시 파일 이름은 임시 파일 경로와 접두사로 생성되고 유일한 이름이 보장됩니다. 이제 이 파일에 데이터를 쓰거나 기존 파일의 데이터를 읽을 수 있습니다.

```PHP
$data = 'This is a sample data to write to the temp file';
$file_handle = fopen($temp_file, 'w');
fwrite($file_handle, $data);
fclose($file_handle);

// Output: 52 (bytes written)
```

또는 기존 파일에서 데이터를 읽을 수 있습니다.

```PHP
$file_handle = fopen($temp_file, 'r');
$data = fread($file_handle, filesize($temp_file));
fclose($file_handle);
echo $data;

// Output: This is a sample data to write to the temp file
```

## 깊이 파고들기

`tempnam()` 함수는 임시 파일을 생성하는 가장 간단한 방법입니다. 하지만 더 세부적으로 제어하고 싶다면 `tmpfile()` 함수를 사용할 수 있습니다. 이 함수는 임시 파일을 메모리에 생성하고 파일 포인터를 반환합니다. 따라서 파일이 생성되지 않거나 다른 프로세스에 의해 열리는 것을 방지할 수 있습니다. 또한 `fopen()` 함수를 사용하여 임시 파일을 열기 전에 `unlink()` 함수를 사용하여 파일을 삭제할 수도 있습니다.

## 가비지 컬렉션

임시 파일 생성 후 반드시 `unlink()` 함수를 사용하여 파일을 삭제하는 것이 좋습니다. 이는 해당 파일이 더 이상 필요하지 않을 때 가비지 컬렉션이 발생하여 디스크 공간을 비울 수 있도록 합니다.

## 보충 자료

- [PHP 공식 문서 - `tempnam()` 함수](https://www.php.net/manual/en/function.tempnam.php)
- [PHP 공식 문서 - `tmpfile()` 함수](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP 공식 문서 - `unlink()` 함수](https://www.php.net/manual/en/function.unlink.php)