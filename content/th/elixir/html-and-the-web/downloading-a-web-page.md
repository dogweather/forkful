---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:34.512785-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Elixir \u0E14\u0E49\u0E27\
  \u0E22\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E48 HTTP client \u0E17\u0E35\u0E48\
  \u0E21\u0E35\u0E1B\u0E23\u0E30\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E20\u0E32\u0E1E\u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E07\u0E32\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\
  \u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22 \u0E19\u0E35\u0E48\u0E04\
  \u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 `HTTPoison`."
lastmod: '2024-03-17T21:57:55.850392-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u0E14\u0E49\u0E27\u0E22\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E48 HTTP client \u0E17\u0E35\u0E48\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E2A\u0E34\u0E17\
  \u0E18\u0E34\u0E20\u0E32\u0E1E\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E07\u0E32\u0E19\u0E19\
  \u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\
  \u0E32\u0E22 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\u0E43\u0E0A\u0E49 `HTTPoison`."
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## วิธีการ:
Elixir ด้วยไลบรารี่ HTTP client ที่มีประสิทธิภาพทำให้งานนี้เป็นเรื่องง่าย นี่คือวิธีการใช้ `HTTPoison`:

```elixir
# ก่อนอื่น, เพิ่ม HTTPoison ไปยัง dependencies ในไฟล์ mix.exs ของคุณ:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# รัน mix deps.get เพื่อดาวน์โหลด dependency ใหม่

# ตอนนี้, ลองดาวน์โหลดหน้าเว็บ:
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Received status code: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# ตัวอย่างการใช้งาน:
{:ok, contents} = PageDownloader.download("http://example.com")
```

ตรวจสอบให้แน่ใจว่าคุณจัดการกับข้อผิดพลาดที่อาจเกิดขึ้นเพื่อหลีกเลี่ยงการขัดข้อง!

## ลงลึก
วิธีการจัดการปฏิสัมพันธ์เว็บของ Elixir ได้รับพลังมาจากความสามารถทางเครือข่ายที่แข็งแกร่งของ Erlang `HTTPoison` เป็นห้องสมุดยอดนิยมที่สร้างอยู่บน `hackney`, แต่ไม่ใช่ผู้เล่นเพียงคนเดียว ยังมี `Tesla`, ซึ่งเสนอวิธีการที่มีโมดูลาร์มากขึ้นพร้อมด้วยการสนับสนุน middleware

ในอดีต, การดาวน์โหลดเนื้อหาเว็บเป็นเรื่องที่ต้องทำด้วยมือมากขึ้น, รวมถึงการสร้างคำขอ HTTP ผ่านการเชื่อมต่อ socket ไลบรารี่ Elixir ปกปิดรายละเอียดเหล่านี้ไว้, ช่วยให้คุณสามารถโฟกัสไปที่ตรรกะการใช้งานแอปพลิเคชันของคุณได้

เมื่อดาวน์โหลดหน้าเว็บ, คุณต้องจัดการกับการปฏิบัติการแบบอะซิงโครนัสและโปรโตคอล HTTP ต่างๆ, ซึ่ง Elixir จัดการได้อย่างสง่างามตามโมเดลคอนเคอเรนซีและการออกแบบที่ทนต่อความผิดพลาด นอกจากนี้, การจัดการข้อมูลข้อความและข้อมูลไบนารีเป็นเรื่องสำคัญ—ตรวจสอบให้แน่ใจว่าคุณพิจารณาถึงการเข้ารหัสและศักยภาพสำหรับข้อมูลไบนารีในเนื้อหาเว็บ

## ดูเพิ่มเติม
- [เอกสาร `HTTPoison`](https://hexdocs.pm/httpoison)
- [ไลบรารี่ `Tesla` บน Hex](https://hex.pm/packages/tesla)
- [คู่มือการใช้งานเกี่ยวกับ OTP Concurrency จาก Elixir School](https://elixirschool.com/en/lessons/advanced/otp-concurrency/)
- [ไลบรารี่ `hackney` ของ Erlang](https://github.com/benoitc/hackney)
