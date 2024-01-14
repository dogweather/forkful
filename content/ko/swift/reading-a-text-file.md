---
title:                "Swift: 텍스트 파일 읽기"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

[위] 

## 왜?

텍스트 파일을 읽는 이유는 여러 가지가 있습니다. 예를 들어, 데이터를 분석하고 가공하려면 원본 데이터를 이해해야합니다. 또는 사용자의 입력을 받아들이거나 순수하게 텍스트 데이터로 처리해야하는 경우가 있을 수 있습니다.

## 어떻게 할까요?

아래의 코드 블록에서는 간단한 Swift 코드를 사용하여 텍스트 파일을 읽고 내용을 출력하는 방법을 보여줍니다. 

```Swift 
// 텍스트 파일의 주소와 경로 설정
let filePath = "myTextFile.txt"

// 파일의 내용을 문자열로 읽기
let fileContents = try String(contentsOfFile: filePath)

// 내용 출력
print(fileContents)
```

위의 코드를 실행하면 `myTextFile.txt`에 저장된 내용이 콘솔에 출력됩니다. 파일의 내용을 받아온 후에는 여러 가지 분석이나 가공을 할 수 있습니다.

## 더 들어가보기

위 코드는 간단한 예제이지만, 실제로는 텍스트 파일을 읽는 것이 조금 더 복잡할 수 있습니다. 파일의 크기가 크거나 특정 문자열 패턴을 찾고 싶은 경우에는 추가적인 작업이 필요할 수 있습니다. 또한 파일의 인코딩이나 에러 처리에 대해 고민해야할 때도 있습니다.

## 더 알아보기

위에서는 간단한 예제만 다루었지만, 실제로는 텍스트 파일을 읽는 것이 조금 더 복잡하고 다양한 요소가 있습니다. 아래 링크를 통해 더 많은 정보를 찾아보세요.

## 더 알아보기

[텍스트 파일을 읽는 방법](https://developer.apple.com/documentation/foundation/data_reading_options) <br>
[파일 처리에 대한 고려사항](https://www.hackingwithswift.com/articles/parsing-json-safely-in-swift) <br>
[Swift의 파일 처리](https://www.raywenderlich.com/830-swift-tutorial-part-2-simple-ios-app-with-table-view)