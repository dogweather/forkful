---
title:    "Arduino: 텍스트 파일 쓰기"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜
텍스트 파일을 작성하는 것이 왜 중요한지 궁금하지 않나요? 만약 아두이노 프로젝트에서 데이터를 저장하고 싶은 경우, 텍스트 파일을 사용하면 쉽고 간단하게 저장할 수 있습니다.

## 코딩 방법
아래의 예시 코드를 따라해보세요.

```Arduino
void setup() {
  // 텍스트 파일의 이름을 설정합니다.
  String fileName = "data.txt";

  // 파일을 쓰기 모드로 열고 데이터를 씁니다.
  File file = SD.open(fileName, FILE_WRITE);
  if (file) {
    file.println("아두이노 프로젝트를 진행하며 데이터를 저장하고 싶다면,");
    file.println("텍스트 파일을 사용하세요.");
    file.close();
  } else {
    Serial.println("파일을 열지 못했습니다.");
  }
}

void loop() {
  // 루프를 계속 반복합니다.
}

```

위의 예시 코드를 실행하면 아두이노가 SD 카드에 "data.txt" 파일을 만들고, 해당 파일에 미리 설정한 데이터를 저장하게 됩니다. 파일에 저장된 내용은 아래와 같습니다.

```
아두이노 프로젝트를 진행하며 데이터를 저장하고 싶다면,
텍스트 파일을 사용하세요.
```

## 깊이 들어가기
텍스트 파일을 작성하는 방법을 조금 더 자세히 알아보겠습니다. 위의 코드에서 `FILE_WRITE`는 파일을 쓰기 모드로 열기 위한 옵션입니다. 이외에도 `FILE_READ`와 같은 다른 옵션이 있습니다. 또한, `println()`은 파일에 새로운 줄을 추가하기 위한 함수입니다. 이 외에도 `print()` 함수를 사용하면 줄 바꿈 없이 문자열을 출력할 수 있습니다.

## 더 알아보기
"아두이노에서 텍스트 파일 작성하기"에 대해 더 많은 정보를 알고 싶다면 아래의 링크를 참조해보세요.

- [아두이노 공식 문서 - SD 라이브러리](https://www.arduino.cc/en/Reference/SD)
- [How to Use SD Card (MicroSD) With Arduino](https://www.instructables.com/id/How-to-Use-SD-Card-Micro-SD-With-Arduino/)
- [Writing to a Text File on an SD Card with Arduino](https://randomnerdtutorials.com/writing-a-text-file-with-arduino/)