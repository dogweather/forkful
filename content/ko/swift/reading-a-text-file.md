---
title:    "Swift: 텍스트 파일 읽기"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 데 참여하는 이유는 여러 가지가 있습니다. 프로그래밍 언어를 배우거나 업무를 수행하는 데 필수적이기 때문이죠. 텍스트 파일을 읽는 방법을 알아 두면 유용하게 사용할 수 있습니다.

## 텍스트 파일 읽기

```Swift
let fileURL = Bundle.main.url(forResource: "sample", withExtension: "txt")
do {
    let contents = try String(contentsOf: fileURL!)
    print(contents)
} catch {
    print("Error reading file.")
}
```

위의 코드는 파일 경로를 가져오고 해당 파일의 내용을 읽어와 출력하는 간단한 예제입니다. 마찬가지로 다른 파일에 대해서도 동일한 방법으로 읽을 수 있습니다.

```
Hello world!
My name is Swift.
This is a sample text file.
```

위 코드를 실행하면, 텍스트 파일의 내용이 출력됩니다.

## 깊이 파고들기

텍스트 파일을 읽는 방법에는 여러 가지가 있습니다. 위에서 살펴본 것은 가장 간단한 방법 중 하나입니다. 하지만 파일의 크기가 큰 경우에는 메모리 문제가 발생할 수 있습니다. 이를 해결하기 위해 나중에는 메모리 효율적인 방법인 스트림을 사용해 볼 수도 있습니다.

## 관련 자료

- [Swift 공식 문서 - 파일 처리](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift 입문서 - 스트림 방식으로 파일 읽기](https://www.notion.so/1dd3bbdeb8374db8ab745bf8702bccc6)