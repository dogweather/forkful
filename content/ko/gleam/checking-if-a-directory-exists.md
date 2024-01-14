---
title:                "Gleam: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

파일이나 디렉터리가 존재하는지 확인하는 것은 프로그래밍에서 널리 사용되는 중요한 작업입니다. 파일을 읽고 쓰기 전에 존재 여부를 확인하고, 존재하지 않는 경우에는 해당 작업을 중단시키는 등 다양한 상황에서 필요합니다. 이번 글에서는 Gleam 프로그래밍 언어를 사용하여 디렉터리가 존재하는지 확인하는 방법을 알려드리겠습니다.

## 하면서

**1. 단순한 방법**

가장 간단한 방법은 `gleam/os` 모듈의 `exists` 함수를 사용하는 것입니다. 이 함수는 파일이나 디렉터리의 존재 여부를 `Boolean` 타입으로 반환해줍니다.

```Gleam
import gleam/os.{exists}

let is_exists = exists("directory/")
// 만약 "directory/"가 존재한다면 `true`가 반환됩니다.
```

**2. 디렉터리인지도 확인하기**

가끔 디렉터리가 아니라 파일을 확인해야 할 때가 있습니다. 이때는 `gleam/os` 모듈의 `is_directory` 함수를 이용하면 됩니다.

```Gleam
import gleam/os.{exists, is_directory}

let directory = "directory/"
let is_exists = exists(directory)
let is_directory = is_directory(directory)
```

위 예제에서 `is_directory` 변수는 `Boolean` 타입으로 디렉터리인지 아닌지를 나타냅니다.

**3. 조금 더 복잡한 방법**

따로 모듈을 사용하지 않아도 `Std.File.Info` 모듈을 사용하여 파일의 `Info`를 받아올 수 있습니다.

```Gleam
import gleam/os.{exists}

let file_info = Std.File.Info.read("directory/")
let is_file_exist = case file {
    Ok(info) -> true
    Error(_error) -> false
}
```

이렇게는 `directory/`의 존재 여부를 확인할 수 있지만, `is_directory` 함수를 사용하지 않기 때문에 디렉터리인지 아닌지는 모릅니다.

## 심도있게 살펴보기

이렇게 쉽게 파일이나 디렉터리의 존재 여부를 확인할 수 있지만, 조금 더 깊게 살펴보면, `gleam/os` 모듈의 함수들은 내부에서 `Std.File` 모듈을 이용하고 있는 것을 알 수 있습니다. 따라서 직접 `Std.File` 모듈의 함수를 사용해도 동일한 결과를 얻을 수 있습니다. `Std.File` 모듈은 파일 및 디렉터리를 효율적으로 다룰 수 있는 다양한 함수들을 제공합니다. 자세한 내용은 공식 문서를 참고하시기 바랍니다.

## 더 알아보기

- [Gleam 공식 문서](https://gleam.run/)
- [Gleam 개발자 블로그](https://medium.com/gleam-lang)
- [Gleam GitHub 리포지토리](https://github.com/gleam-lang/gleam)