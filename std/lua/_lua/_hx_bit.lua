local _hx_bit
pcall(require, 'bit32') pcall(require, 'bit')
if type(jit) == 'table' or _G._VERSION == "Lua 5.3" then
  local _hx_bit_raw = bit or bit32
  _hx_bit = setmetatable({},{__index = function(t,k) return function(...) return _hx_clamp(rawget(_hx_bit_raw,k)(...)) end end})
else
  _hx_bit = bit or bit32
end
