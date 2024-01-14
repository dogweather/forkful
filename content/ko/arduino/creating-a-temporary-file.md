---
title:                "Arduino: 임시 파일 만들기"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜
임시 파일을 만드는 것에 대해 아두이노 프로그램을 공부하게 되는 이유는 무엇일까요? 임시 파일은 프로그래밍 작업에서 자주 사용되는 유용한 도구입니다. 여러분이 만든 프로그램에서 다른 프로그램과 서로 데이터를 주고받거나 저장할 때, 임시 파일이 사용되지 않을 수 없습니다. 따라서 아두이노에서 임시 파일을 만드는 방법을 배워두면 프로그래밍 작업을 보다 쉽고 효율적으로 수행할 수 있습니다.

# 어떻게
아두이노에서 임시 파일을 만드는 방법은 매우 간단합니다. 먼저 파일 함수를 선언하고 변수를 설정해야 합니다. 그리고 파일을 생성하고 데이터를 쓸 수 있도록 파일을 연 다음, 임시 파일을 만들어줍니다. 아래 코드를 참고하면 더 쉽게 이해하실 수 있을 것입니다.

\`\`\`Arduino

#include <SD.h> // 파일함수 불러오기

File myFile; // 파일 변수 설정

void setup() {
  Serial.begin(9600); // 시리얼 모니터 사용
  
  if (!SD.begin(4)) { // 파일을 읽고 쓸 수 있도록 초기화
    Serial.println("SD 카드 오류");
    return;
  }
  
  myFile = SD.open("temp.txt", FILE_WRITE); // 파일 생성
  myFile.println("Hello, World!"); // 파일에 데이터 쓰기
  myFile.close(); // 파일 닫기
}

void loop() {
  // 아두이노에서 다른 작업 수행
}

\`\`\`

위 코드를 실행하면 SD 카드에 "tmp.txt"라는 임시 파일이 생성될 것입니다. 이렇게 생성된 임시 파일은 데이터를 주고받는데 자주 사용될 수 있으며, 매우 유용한 것입니다.

# 깊이 파고들기
아두이노에서 임시 파일을 만드는 동작은 내부적으로 작은 메모리 공간을 사용합니다. 따라서 프로그램 내에서 너무 많이 사용되거나 반복적으로 생성하게 되면 컴퓨터의 성능에 영향을 미칠 수 있습니다. 임시 파일을 사용할 때에는 이러한 점을 유의하여 최적의 코드를 작성하는 것이 중요합니다. 또한 임시 파일의 사용이 끝난 후에는 꼭 파일을 닫아주는 것이 좋습니다. 이렇게 하지 않으면 파일을 닫지 않고 임시 파일을 남겨두면 컴퓨터의 성능에도 영향을 줄 수 있고, SD 카드의 용량 또한 낭비될 수 있기 때문입니다.

# 활용하기
- <https://www.arduino.cc/en/Reference/SD>
- <https://www.arduino.cc/en/Tutorial/Files>
- <https://programmingelectronics.com/how-to-program-arduino-with-your-sd-card/>
- https://www.instructables.com/id/Arduino-Temp-File-System-With-Tiny-FAT/>
# 관련 링크
- <https://www.arduino.cc/>
- <https://www.arduino.cc/en/Tutorial/HomePage>
- <https://www.arduino.cc/reference/en/>
- <https://www.arduino.cc/en/Reference/Libraries>