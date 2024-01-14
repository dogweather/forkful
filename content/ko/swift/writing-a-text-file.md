---
title:                "Swift: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 이유는 다양합니다. 예를 들어, 데이터를 저장하고 관리하는데 사용하거나, 프로그램이 실행될 때 설정 파일을 로드하고 읽는데 사용할 수 있습니다. 또는 간단한 메모나 노트를 저장하는 용도로도 사용할 수 있습니다.

## 작성 방법

우선 ```write(to: String, atomically: Bool, encoding: String.Encoding)``` 메서드를 사용하여 텍스트 파일을 작성할 수 있습니다. 이 메서드는 세 가지 매개 변수를 가지고 있습니다. 첫 번째는 파일이 저장될 경로이고, 두 번째는 원자적으로 쓸 것인지를 나타내는 부울 값이고, 마지막으로 파일의 인코딩 방식을 나타내는 ```String.Encoding``` 열거형입니다.

예를 들어, 아래의 코드는 "sample.txt"라는 파일을 생성하고, "Hello, World!"라는 문자열을 저장하며, 파일의 인코딩 방식을 UTF-8로 설정합니다.

```Swift
let filePath = "sample.txt"
let text = "Hello, World!"
do {
    try text.write(to: filePath, atomically: true, encoding: .utf8)
    print("파일이 성공적으로 작성되었습니다.")
} catch {
    print("파일 작성에 실패하였습니다.")
}
```

파일을 성공적으로 작성하면 콘솔에 "파일이 성공적으로 작성되었습니다."라는 메시지가 출력될 것입니다. 터미널에서 파일을 확인해보면 "Hello, World!" 라는 문자열이 저장된 것을 볼 수 있습니다.

## 깊게 들어가기

위의 예제에서는 원자성 옵션을 ```true```로 설정하였습니다. 이 옵션은 파일을 원자적으로 쓸 것인지를 나타내는데, 즉 파일이 전부 쓰여진 후에 디스크에 저장되도록 하는 옵션입니다. 이를 사용하면 파일이 중간에 손상되는 경우를 방지할 수 있습니다.

또한 파일의 인코딩 방식을 설정하여 다양한 언어나 문자를 저장할 수 있습니다. UTF-8은 가장 많이 사용되는 인코딩 방식 중 하나이며, 다른 인코딩 방식들도 있으니 필요에 따라 선택하면 됩니다.

## 관련 문서들

- [String - Apple 개발자 문서](https://developer.apple.com/documentation/swift/string)
- [String.Encoding - Apple 개발자 문서](https://developer.apple.com/documentation/swift/string/encoding)