---
title:                "임시 파일 생성하기"
html_title:           "Gleam: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 것에 참여하는 이유는 다양합니다. 일시적으로 파일을 필요로 하는 경우, 데이터를 임시로 저장하거나 프로세스를 관리하기 위해 사용할 수 있습니다.

## 어떻게

임시 파일을 생성하는 것은 Gleam의 `os.temp_dir()`와 `os.temp_file()` 함수를 사용하여 간단하게 할 수 있습니다.

```
Gleam.import os/temp

let temp_dir = os.temp_dir()
let temp_file = os.temp_file()

io.print("임시 디렉토리 경로: {}", [temp_dir])
io.print("임시 파일 경로: {}", [temp_file])
```

출력:

```
임시 디렉토리 경로: /tmp
임시 파일 경로: /tmp/12345
```

## 깊게 파헤치기

임시 파일을 생성할 때 고려해야 할 요소가 있습니다. 먼저, 임시 파일이 생성될 위치를 지정할 수 있습니다. `os.temp_file()` 함수에 옵션으로 경로를 전달하여 원하는 디렉토리에 임시 파일을 생성할 수 있습니다. 또한, 임시 파일의 이름을 직접 정할 수도 있습니다. 이를 위해서는 `os.temp_file()` 함수에 옵션으로 이름을 전달하면 됩니다. 또한, 임시 파일을 생성하고 사용한 후에는 꼭 삭제해야 합니다. 이를 위해 `os.temp_file()` 함수를 사용하지 않고, `os.temp_dir()` 함수를 사용하여 임시 디렉토리를 생성한 후 임시 파일을 생성하고 임시 디렉토리를 삭제하는 방법도 있습니다.

## 여기서 더 알아보기

더 많은 정보를 알고 싶다면, 다음 링크들을 참고해보세요.

[os module - Gleam 공식 문서] (https://gleam.run/documentation/stdlib/os.html#temp_dir)
[일시 파일 임시 저장소 생성 방법 - Gleam 블로그] (https://gleam.run/blog/temporary-file-creation-how-to)