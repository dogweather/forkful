---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

임시 파일을 만드는 것은 일시적인 데이터 저장용으로 사용되는 파일을 생성하는 작업입니다. 프로그래머들은 디버깅, 소프트웨어 구성의 일시적 변화, 데이터 로스를 예방하는 등의 이유로 이를 수행합니다. 

## 작성 방법:

우선 본인의 아두이노 보드에 SD 카드 모듈을 부착하고 SD 카드를 넣습니다. 다음의 코드 스니펫은 "temp.txt"라는 이름의 임시 파일을 생성하게 됩니다. 

```Arduino
#include <SD.h>

File myFile;

void setup() {
    Serial.begin(9600);
    if (!SD.begin(4)) {
        Serial.println("SD card initialization failed!");
        return;
    }
    myFile = SD.open("temp.txt", FILE_WRITE);
    if (myFile) {
        myFile.close();
        Serial.println("temp.txt has been created!");
    } else {
        Serial.println("Error opening temp.txt");
    }
}

void loop() {

}
```

이 코드를 실행하면, Serial Monitor에서 "temp.txt has been created!"라는 메세지를 확인할 수 있습니다.

## 깊이 있게 알아보기:

임시 파일 생성은 오래전부터 컴퓨팅 시스템에서 사용되던 기술입니다. 최초의 사용용도는 메모리 리소스가 부족한 환경에서 사용되었습니다. 

다른 방법 중 하나로는 RAM에 임시 파일을 생성하는 방법이 있습니다. 이 방법은 저장 공간에 접근하는 시간을 단축시키지만, 파일이 손실될 위험이 있으니 주의가 필요합니다.

아두이노에서 임시 파일을 생성하기 위해서는 주로 SD 카드와 같은 외부 저장공간을 사용하여 파일을 씁니다. 때문에 SD 라이브러리를 쓰는 것입니다. 

## 참고 자료:

- 아두이노 SD 카드 모듈 사용 법: https://www.arduino.cc/en/Reference/SD 
- 임시 파일에 대해서 좀 더 알아보기: https://en.wikipedia.org/wiki/Temporary_folder
- SD 라이브러리 GitHub: https://github.com/arduino-libraries/SD