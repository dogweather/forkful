---
title:    "Gleam: 디렉토리가 존재하는지 확인하는 방법"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하다보면, 디렉토리가 존재하는지 확인하는 일이 많이 발생합니다. 이 과정에서 디렉토리가 없는 경우 에러를 방지하고, 디렉토리가 있는 경우 추가적인 작업을 수행할 수 있습니다. 

## 하우 투

```Gleam
let my_dir = "/path/to/directory"

if Filesystem.exists(my_dir) {
  // 디렉토리가 존재할 때 수행할 코드
  io.println("디렉토리가 존재합니다: " ++ my_dir)
} else {
  // 디렉토리가 존재하지 않을 때 수행할 코드
  io.println("디렉토리가 존재하지 않습니다.")
}
```

위 예제는 파일 시스템 모듈의 `exists` 함수를 사용하여 디렉토리가 존재하는지 확인하는 방법을 보여줍니다. 파일 시스템 모듈은 기본 라이브러리이기 때문에 별도로 가져오거나 설치할 필요가 없습니다.

## 딥 다이브

디렉토리가 존재하는지 확인하는 과정에서 파일 시스템의 여러 다른 함수들을 이용할 수 있습니다. `exists` 함수는 단순히 디렉토리가 존재하는지 여부만을 확인하지만, 파일의 존재 여부를 확인하는 함수(`Filesystem.file_exists`)나 새로운 디렉토리를 만드는 함수(`Filesystem.mkdir`) 등 다양한 함수를 이용할 수 있습니다.

## 참고

- [Gleam 공식 문서 - 파일 시스템 모듈](https://gleam.run/documentation/standard_library/#filesystem)
- [Gleam 공식 문서 - 파일 시스템 관리](https://gleam.run/documentation/guides/file_system_management/)