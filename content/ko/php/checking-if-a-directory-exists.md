---
title:                "PHP: 디렉터리가 존재하는지 확인하기"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것은 컴퓨팅에서 중요한 작업입니다. 여러분이 알고 싶은 디렉토리가 실제로 존재하는지 여부를 확인하기 위해, 이 기능은 유용하게 사용될 수 있습니다.

## 어떻게

```PHP
if (is_dir('/home/user/documents')) {
  echo "디렉토리가 존재합니다.";
} else {
  echo "해당 디렉토리는 존재하지 않습니다.";
}
```
위의 예시에서처럼, PHP의 `is_dir()` 함수는 특정 디렉토리가 존재하는지를 확인할 수 있습니다. 디렉토리가 존재하면, `true`를 반환하고 아니면 `false`를 반환합니다. 이를 통해 여러분은 디렉토리가 존재하는지를 빠르고 쉽게 확인할 수 있습니다.

또한, `is_dir()` 함수는 상대 경로나 절대 경로 모두 입력으로 받아들일 수 있으며, 파일 시스템의 상태를 변경하지 않습니다. 또한, 이 함수는 매개변수로 받은 디렉토리의 존재 여부를 확인하는 것에 그치지 않고, 해당 디렉토리가 실제로 디렉토리인지를 확인하여 `is_directory()` 함수로 반환합니다.

## 딥 다이브

디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 매우 중요한 작업입니다. 예를 들어, 여러분의 파일 시스템을 조작하여 특정 디렉토리 내부의 모든 파일을 검색하고 수정하고 싶다고 가정해 봅시다. 이를 위해서는 우선 해당 디렉토리가 실제로 존재하는지를 확인하는 것이 필수적입니다. 따라서 디렉토리가 존재하는지를 확인하는 것은 프로그래밍에서 아주 중요한 작업이라 할 수 있습니다.

## 참고

- [PHP 공식 문서 - is_dir()](https://www.php.net/manual/en/function.is-dir.php)
- [Ten Factors - 디렉토리가 존재하는지 확인하기(is_dir())](https://twisted1919.tistory.com/23)