#!/usr/bin/env ruby

require 'date'

# Check for proper usage
if ARGV.length != 1
  puts "Usage: #{$PROGRAM_NAME} <filename>"
  exit 1
end

files = ARGV[0]

# Ensure the file exists
unless File.file?(files)
  puts "File not found: #{files}"
  exit 1
end

begin
  open(files, 'r').readlines.each do |filename|
    # Read the file content
    content = File.read(filename)

    # Define your regex pattern and replacement text
    # Example: Replace 'old_text' with 'new_text'
    pattern     = /(title:.+?\n)/m
    replacement = "\\1date:                  2024-01-19\n"

    # Perform the regex replacement
    new_content = content.sub(pattern, replacement)

    # Write the changes back to the file
    File.open(filename, 'w') { |file| file.write(new_content) }

    puts "#{filename} updated successfully."
  end
rescue => e
  puts "An error occurred: #{e.message}"
end
