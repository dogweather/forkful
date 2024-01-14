---
title:    "Gleam: 텍스트 파일 읽기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

 이 글을 읽는 이유는 텍스트 파일을 읽는 방법을 배우기 위함입니다. 텍스트 파일은 컴퓨터에서 중요한 역할을 합니다. 그렇기 때문에 이를 읽는 방법을 알고 있다면 컴퓨터 프로그래밍에 많은 도움이 될 것입니다.

## How To

Gleam에서 텍스트 파일을 읽는 방법은 매우 쉽습니다. 다음의 코드를 따라하면 됩니다.

```
Gleam.text_file.read("example.txt") // "example.txt"라는 텍스트 파일을 읽습니다.
|> Gleam.text_file.lines() // 각 줄을 Gleam 리스트로 반환합니다.
|> Gleam.text.join("\n") // 리스트의 모든 요소를 하나의 문자열로 합칩니다.
```

위의 코드를 실행하면 "example.txt" 파일의 모든 내용을 하나의 문자열로 반환하는 것을 볼 수 있습니다.

### 샘플 출력

```
This is the first line
This is the second line
This is the third line
```

## Deep Dive

텍스트 파일을 읽는 것은 컴퓨터에서 매우 중요한 작업 중 하나입니다. Gleam은 이 작업을 효율적이고 쉽게 할 수 있도록 도와줍니다. Gleam은 다른 언어들과 달리 텍스트 파일을 읽을 때에도 함수형 프로그래밍을 적용할 수 있도록 하기 때문에 더욱 유용합니다.

참고로, Gleam은 텍스트 파일을 간단하게 쓸 수 있는 함수들도 제공합니다. `Gleam.text_file.write()` 와 `Gleam.text_file.append()`라는 함수들이 그것입니다. 이 함수들은 파일의 내용을 쓰거나 기존 내용 다음에 내용을 추가할 수 있습니다.

# See Also

- [Gleam 공식 문서](https://gleam.run)
- [Gleam 텍스트 파일 관련 API](https://gleam.run/modules/text_file.html)