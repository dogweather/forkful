---
title:                "디렉터리의 존재 여부 확인하기"
html_title:           "Swift: 디렉터리의 존재 여부 확인하기"
simple_title:         "디렉터리의 존재 여부 확인하기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

--- 
## 왜
누군가 디렉토리가 존재하는지 확인하는 것에 참여하는 이유는, 코드의 실행 중 간혹 디렉토리가 없는 상황에서 코드가 오류 없이 계속 진행하기 위해서입니다.

## 코딩하는 방법
```Swift
if FileManager.default.fileExists(atPath: "Documents") {
    print("Documents 디렉토리가 존재합니다.")
} else {
    print("Documents 디렉토리가 존재하지 않습니다.")
}

```
### 예시 출력
```
Documents 디렉토리가 존재하지 않습니다.
```

## 깊이 파고들기
디렉토리의 존재 여부를 확인하는 것은 코드의 안전성을 높이는 중요한 부분입니다. 만약 디렉토리가 존재하지 않는 상황에서 해당 디렉토리로 파일을 생성하려고 한다면, 예기치 않은 오류가 발생할 수 있습니다. 이를 방지하기 위해 항상 디렉토리의 존재 여부를 체크하는 것이 좋습니다.

## 또 다른 정보
### [Swift FileManager Documentation](https://developer.apple.com/documentation/foundation/filemanager)
### [How to create and check for an existing directory using Swift](https://medium.com/@sauvik_dolui/how-to-create-and-check-for-an-existing-directory-using-swift-4-7089fa597f13)