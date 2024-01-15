---
title:                "임시 파일 만들기"
html_title:           "Arduino: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
Temp 파일을 만드는 이유는 다음과 같습니다: 간단한 처리를 위해서 기존 파일에 영향을 주지 않고 원본 파일을 백업하는 것입니다.

## 작동 방법
Temp 파일을 만드는 것은 아두이노에서 아주 쉽게 할 수 있습니다. 아두이노에서 제공하는 ```File``` 함수를 사용하여 다음과 같은 코드를 작성할 수 있습니다:

```Arduino
File tempFile = SD.open("temp.txt", FILE_WRITE);
if (tempFile) {
  tempFile.println("Temp 파일에 쓰는 내용");
  tempFile.close();
}
```

위의 코드는 SD 카드에 temp.txt라는 파일을 생성하고, 쓰기 모드로 열어 "Temp 파일에 쓰는 내용"이라는 내용을 쓴 후 파일을 닫습니다.

## 깊이 파헤치기
위의 예제에서 사용된 ```SD.open()``` 함수는 SD 카드에 파일을 생성하고 열기 위한 함수입니다. 이 함수는 다음과 같은 매개 변수를 가질 수 있습니다:

- ```filename``` : 생성하고 열 파일의 이름입니다.
- ```filemode``` : 파일을 열 때 사용할 모드를 나타내는 매개 변수입니다. 위의 예제에서 사용한 ```FILE_WRITE```는 쓰기 모드를 나타냅니다.
- ```flags``` : 매개 변수에는 파일을 열 때 적용할 플래그를 지정할 수 있습니다. 예를 들어 파일을 생성하고 싶지 않을 때는 ```O_CREAT``` 플래그를 사용할 수 있습니다.

위의 예제에서는 SD 카드를 사용하여 temp.txt 파일을 생성하고 열었지만, 다른 장치나 메모리에도 비슷한 방법으로 임시 파일을 생성할 수 있습니다. 각 장치나 메모리에 따라 조금씩 다를 수 있으니 관련 자료를 참고하시기 바랍니다.

## 관련 자료
- [Arduino SD 라이브러리 문서](https://www.arduino.cc/en/Reference/SD)
- [OS X Developer Library - 임시 파일 생성 및 사용](https://developer.apple.com/library/content/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html#//apple_ref/doc/uid/TP40010672-CH2-SW13)