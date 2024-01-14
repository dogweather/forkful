---
title:                "Arduino: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 왜 HTTP 요청을 보내는가?

HTTP 요청을 보내는 것은 웹 서비스와의 통신이 필요한 경우에 매우 유용합니다. 예를 들어, IoT 디바이스를 웹 서비스에 연결하려면 HTTP 요청을 사용해야 합니다. 이를 통해 디바이스에 저장된 데이터를 웹 서비스에 전송하거나 웹 서비스에서 디바이스를 제어할 수 있습니다.

# 어떻게 HTTP 요청을 보낼 수 있을까?

Arduino에서 HTTP 요청을 보내려면 다음의 라이브러리가 필요합니다:
* ESP8266WiFi - WiFi 모듈 제어를 위한 라이브러리
* WiFiClient - 웹 서비스와의 통신을 위한 클라이언트 라이브러리
* EEPROM - 데이터를 저장하기 위한 EEPROM 라이브러리 (선택 사항)

아래 예제에서는 OpenWeatherMap API에서 현재 날씨 정보를 가져와 디스플레이에 출력하는 예시를 보여줍니다.

```Arduino
#include <ESP8266WiFi.h>
#include <WiFiClient.h>
#include <EEPROM.h>

// Wi-Fi 연결 정보
const char* ssid = "Your WiFi SSID";
const char* password = "Your WiFi Password";

// API 요청을 보낼 서버 주소
const char* server = "api.openweathermap.org";

// API 요청에 사용할 도시 ID (Seoul)
const int cityId = 1835848;

// API 요청에 사용할 API 키 (본인의 키로 교체)
const char* apiKey = "Your API Key";

// 데이터 저장을 위한 EEPROM 주소
const int eepromAddr = 0;

void setup() {
  // 시리얼 통신 시작
  Serial.begin(9600);

  // EEPROM 초기화
  EEPROM.begin(512);

  // Wi-Fi 연결
  Serial.print("Connecting to ");
  Serial.println(ssid);
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("Connected!");

  // EEPROM에 저장된 API 키 읽기
  char storedApiKey[32];
  for(int i=0; i<32; i++){
    storedApiKey[i] = EEPROM.read(eepromAddr + i);
  }
  // 저장된 API 키와 입력한 API 키가 같은지 확인
  if(strcmp(apiKey, storedApiKey)){
    // 다를 경우 EEPROM에 저장된 API 키로 교체 후 저장
    strcpy(apiKey, storedApiKey);
    for(int i=0; i<32; i++){
      EEPROM.write(eepromAddr + i, apiKey[i]);
    }
    EEPROM.commit();
    Serial.println("New API key saved to EEPROM");
  }
}

void loop() {
  // 날씨 정보를 저장할 변수 선언
  float temp, humidity, pressure;

  WiFiClient client;
  // 웹 서비스에 연결
  if (client.connect(server, 80)) {
    // API 요청을 위한 URL 생성
    String url = "/data/2.5/weather?id=" + String(cityId) + "&units=metric&APPID=" + String(apiKey);

    // 서버에 요청 보내기
    client.println("GET " + url + " HTTP/1.1");
    client.println("Host: " + String(server));

    // 서버 응답 읽기
    while(client.connected()){
      String line = client.readStringUntil('\n');
      if(line == "\r"){
        // 응답 헤더가 끝나면 바로 다음 줄부터 데이터가 옴
        line = client.readStringUntil('\n');
        // 온도, 습도, 기압 정보 파싱
        // JSON 형식: {"main":{"temp":20.65,"humidity":25,"pressure":1018.81}}
        temp = line.substring(line.indexOf("temp")+6, line.indexOf(","));
        humidity = line.substring(line.indexOf("humidity")+10, line.indexOf(", \"pressure"));
        pressure = line.substring(line.indexOf("pressure")+10,