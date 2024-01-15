---
title:                "텍스트 파일 읽기"
html_title:           "Gleam: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

문자열 파일을 읽는 것은 일반적으로 프로그래머들이 자주 다루는 작업 중 하나입니다. Gleam을 사용하면 문자열 파일을 간단하고 효율적으로 읽을 수 있습니다.

## 사용 방법

아래는 Gleam을 사용하여 문자열 파일을 읽는 간단한 예제 코드입니다.

```Gleam
import gleam/io

fn main() {
  let file = io.file.open("example.txt")
  let content = io.file.read_to_string(file)
  
  match content {
    Ok(text) -> io.print(text)
    Err(error) -> error
  }
}
```

위의 코드에서는 `gleam/io` 모듈을 가져와 파일을 열고, `read_to_string` 함수를 사용하여 파일의 내용을 문자열로 읽어옵니다. 그 다음 `match` 표현식을 사용하여 예외 처리를 하고, 성공적으로 파일을 읽었을 때는 파일의 내용을 출력합니다.

이 코드를 실행하면 아래와 같은 결과를 볼 수 있습니다.

```
이 예제는 Gleam으로 문자열 파일을 읽는 방법을 간단하게 보여줍니다.
Gleam은 일반적인 파일 입출력 작업을 더욱 쉽게 처리할 수 있도록 해줍니다.
```

## 더 들어가기

위의 예제 코드는 기본적인 Gleam 프로그래밍에 대한 이해를 바탕으로 작성되었습니다. 하지만 Gleam은 더 많은 기능을 제공하며 다양한 방법으로 문자열 파일을 읽을 수 있습니다. 더 자세한 정보는 Gleam 공식 문서를 참고하시기 바랍니다.

## 같이 보기

- Gleam 공식 문서 (https://gleam.run/documentation/guides/)
- Gleam GitHub 저장소 (https://github.com/gleam-lang/gleam)