---
title:                "텍스트 파일 작성하기"
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 쓰는 것이 왜 중요한지 궁금하지 않으세요? 텍스트 파일은 여러분의 프로그램에서 데이터를 저장하고 읽는 데에 유용합니다. 이것은 여러분이 프로그램을 처음부터 다시 작성하지 않아도 되게 해주는 매우 유용한 도구입니다.

## 어떻게 하나요?

우리는 여러분이 텍스트 파일을 만드는 방법을 알려드릴 거에요. 아래의 예제 코드를 보고 따라 해보세요.

```Arduino
// 파일 객체를 만듭니다.
File myFile;

void setup() {
  // SD 카드 초기화
  SD.begin();

  // 파일 이름과 모드(쓰기)를 넣고 파일을 엽니다.
  myFile = SD.open("test.txt", FILE_WRITE);

  // 만약 파일이 열리지 않으면 오류를 출력합니다.
  if (myFile) {
    // "Hello world"를 파일에 작성합니다.
    myFile.println("Hello world");
    
    // 파일을 닫습니다.
    myFile.close();

    // 완료 메시지를 출력합니다.
    Serial.println("파일이 성공적으로 작성되었습니다.");
  } else {
    // 만약 파일이 열리지 않으면 오류 메시지를 출력합니다.
    Serial.println("오류: 파일을 열 수 없습니다.");
  }
}

void loop() {
  
}
```

위 코드를 올바르게 실행하면 SD 카드에 "text.txt" 파일이 생성되고 "Hello world"가 기록될 것입니다.

## 깊게 들어가보기

텍스트 파일을 쓰는 더 많은 방법과 파일을 열고 닫는 방법 등에 대해 더 알고 싶다면 아래의 링크를 참조해주세요.

- [Arduino Reference](https://www.arduino.cc/reference/en/libraries/sd/)
- [Tutorial: SD Cards and Arduino](https://www.circuito.io/blog/arduino-sd-card-file-i-o/)
- [Writing to a text file on an SD card with Arduino](https://create.arduino.cc/projects/1283/writing-to-a-text-file-on-an-sd-card-with-arduino)

## 같이 보기

- [Markdown 사용법](https://gist.github.com/ihoneymon/652be052a0727ad59601)
- [강력한 텍스트 편집기를 사용하여 Markdown 미리보기하기](https://macdown.uranusjr.com/)