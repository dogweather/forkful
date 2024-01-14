---
title:    "Gleam: 표준 에러에 쓰는 방법"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

코딩 중에 때때로 오류를 처리해야 할 때가 있습니다. 이때, 쓰이게 되는것이 바로 "standard error" 입니다. 이 포스트에서는 standard error에 대해 알아보고, 이를 어떻게 사용하는지 살펴보겠습니다.

## 사용 방법

우선 Gleam에서 standard error을 쓰는 방법을 알아보겠습니다. 아래의 코드 블록을 참고해주세요.

```Gleam
import procedure.error_logger
import gleam/io

fn main() {
  let message = "Hello, 에러!";
  let error = ErrorLogger.error(message);
  IO.write_lines(std.err, [error])
}
```

위의 코드에서 `ErrorLogger.error`를 호출하여 오류 메시지를 만들고, `IO.write_lines`를 사용하여 `std.err`을 통해 콘솔에 출력합니다. 이렇게 하면 콘솔에 오류 메시지가 표시될 것입니다.

## 자세히 알아보기

"standard error"은 오류 메시지를 콘솔에 출력하는데 사용됩니다. 이것은 `std.err`이라는 특별한 파일 핸들을 통해 이루어집니다. 코드에서는 `IO.write_lines`를 사용하여 오류를 표시하는데, 이 함수는 문자열의 배열을 받아 해당 배열의 모든 요소를 새 줄로 쓰도록 작성되어 있습니다. 그 말은 즉슨, `std.err`에 출력할 메시지는 배열 형태로 전달되어야 한다는 것입니다.

## 관련 링크 보기

- Gleam 공식 문서: https://gleam.run/book/tour/error-handling.html
- Standard output & standard error 관련 포스트: https://www.tutorialspoint.com/unix/unix-io-redirections.htm
- `IO.write_lines`의 공식 문서: https://gleam.run/docs/stdlib/gleam#write_lines