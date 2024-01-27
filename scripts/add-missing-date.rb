#!/usr/bin/env ruby

require 'date'

# Check for proper usage
if ARGV.length != 1
  puts "Usage: #{$PROGRAM_NAME} <filename>"
  exit 1
end

filename = ARGV[0]

# Ensure the file exists
unless File.file?(filename)
  puts "File not found: #{filename}"
  exit 1
end

begin
  # Read the file content
  content = File.read(filename)

  # Define your regex pattern and replacement text
  # Example: Replace 'old_text' with 'new_text'
  pattern     = /^---\n/m
  replacement = "---\ndate:                  #{Date.today.strftime('%Y-%m-%d')}\n"

  # Perform the regex replacement
  new_content = content.sub(pattern, replacement)

  # Write the changes back to the file
  File.open(filename, 'w') { |file| file.write(new_content) }

  puts "File updated successfully."

rescue => e
  puts "An error occurred: #{e.message}"
end
