---
title:                "Arduino: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것에 대해 궁금한 사람들은 코딩과 Arduino에 대해 더 많이 배우고 싶을 것입니다.

# 어떻게

텍스트 파일을 읽는것은 매우 쉬운 작업입니다. 다음 예제 코드를 따라해보세요.

```Arduino 
File myFile;
void setup() {
  Serial.begin(9600); // 시리얼 모니터를 엽니다.
  myFile = SD.open("example.txt"); // 텍스트 파일을 엽니다.
  if (myFile) { // 파일이 열렸는지 확인합니다.
    while (myFile.available()) { // 파일이 읽기 가능한지 확인합니다.
      Serial.write(myFile.read()); // 파일의 모든 내용을 읽어서 시리얼 모니터에 출력합니다.
    }
    myFile.close(); // 파일을 닫습니다.
  }
}
void loop() {
  // 아무것도 하지 않습니다.
}
```

위 코드를 실행하면, "example.txt" 파일의 모든 내용이 시리얼 모니터에 출력됩니다.

## 깊은 이해

텍스트 파일을 읽기 위해서는 Arduino에 SD(Secure Digital) 라이브러리를 추가해야 합니다. 이 라이브러리는 Arduino SD 카드 모듈로부터 파일을 읽거나 쓰는 기능을 제공합니다.

또한 SD 라이브러리와 함께 사용하기 위해서는 FAT 파일 시스템도 필요합니다. FAT 파일 시스템은 디지털 카메라나 MP3 플레이어와 같은 디바이스에서도 널리 사용되는 파일 시스템입니다.

즉, SD 라이브러리를 사용하면 Arduino에서 SD 카드 모듈을 통해 텍스트 파일을 읽을 수 있게 됩니다.

# 같이 보기

- [Arduino SD 라이브러리 공식 문서](https://www.arduino.cc/en/Reference/SD)
- [FAT 파일 시스템에 대한 이해](https://www.tldp.org/LDP/tlk/fs/filesystem.html)