---
title:                "텍스트 파일 읽기"
html_title:           "Swift: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
여러분은 아마도 프로그래밍에서 텍스트 파일을 읽는 것을 들어본 적이 있을 것입니다. 그렇다면 텍스트 파일을 읽는 것은 무엇일까요? 간단하게 말하자면, 텍스트 파일을 읽는 것은 컴퓨터에서 텍스트를 읽어오는 것을 말합니다. 그리고 이것을 왜 프로그래머들이 하는 걸까요? 텍스트 파일은 프로그램에 필요한 데이터를 저장하는 중요한 방법 중 하나이기 때문입니다.

## How to:
텍스트 파일을 읽는 것은 Swift에서 아주 간단한 작업입니다. 아래의 코드를 따라해 보세요! 

```Swift
// 먼저 해당 파일을 읽어와서 NSString 형태로 저장합니다.
let content = try String(contentsOfFile: "파일경로")
// 그 다음, 읽어온 텍스트를 출력합니다.
print(content)
```

이렇게 하면 해당 파일의 내용이 출력될 것입니다. 이렇듯 Swift에서는 텍스트 파일을 읽는 것이 매우 간단하며, 문자열 형태로 읽어와서 필요한 작업을 할 수 있습니다.

## Deep Dive:
텍스트 파일을 읽는 것은 오래된 기술입니다. 이전에는 바이너리 형태의 파일을 읽는 것이 일반적이었으나, 바이너리 파일은 컴퓨터마다 다른 형식으로 저장되기 때문에 읽는 것이 어려웠습니다. 이에 따라 텍스트 파일이 사용되기 시작했는데, 이는 모든 컴퓨터에서 동일한 형태로 읽을 수 있기 때문입니다.

또한 텍스트 파일은 여러 가지 방법으로 읽고 처리할 수 있습니다. 위에서 보여드렸던 코드 외에도, 더 많은 기능을 포함한 다양한 라이브러리를 이용할 수 있습니다. 따라서 프로그래머들은 자신에게 가장 적합한 방법으로 텍스트 파일을 읽고 처리할 수 있습니다.

## See Also:
텍스트 파일을 읽는 방법에 대해 더 자세히 알고 싶다면, 다음 링크를 참고해 보세요.

https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292

또한 파일 처리에 대해 더 알고 싶다면, 다음의 링크를 확인해 보세요.

https://docs.swift.org/swift-book/LanguageGuide/FilesAndDirectories.html